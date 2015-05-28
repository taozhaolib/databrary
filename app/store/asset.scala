package store

import java.io.{File,FileNotFoundException}
import java.nio.{ByteBuffer,channels}
import java.nio.file.{StandardOpenOption,FileSystemException,FileAlreadyExistsException}
import scala.concurrent.Future
import play.api.Play.current
import play.api.libs.Files.TemporaryFile
import macros._
import dbrary._
import site._
import models._

private[store] sealed abstract class StoreDir(conf : String) {
  protected val baseDir = {
    val c = current.configuration
    new File(c.getString(conf).getOrElse(throw c.globalError("Missing configuration for " + conf)))
  }
}

object Stage extends StoreDir("store.stage") {
  def file(f : File) : File =
    if (f.isAbsolute) f else new File(baseDir, f.getPath)
  def file(s : String) : File =
    file(new File(s))
  def path(f : File) : String =
    f.getPath.stripPrefix(baseDir.getPath + '/')

  private val transcodedRegex = "(.*)/transcoded/(.*)-01.mp4".r
  /** Locate the original and transcoded files. */
  def findTranscoded(f : File) : Option[(Option[File],File)] = f.getPath match {
    case transcodedRegex(dir, base) => Some(({
      val t = new File(dir, base + ".")
      val l = t.getParentFile.listFiles(new java.io.FilenameFilter {
        def accept(d : File, name : String) = name.startsWith(t.getName)
      })
      if (l == null || l.length != 1) None
      else Some(l.head)
    }, f))
    case _ =>
      val t = new File(new File(f.getParentFile, "transcoded"),
        f.getName.replaceFirst("\\.[a-zA-Z0-9_]+$", "") + "-01.mp4")
      if (t.isFile) Some((Some(f), t))
      else None
  }
}

object Upload extends StoreDir("store.upload") {
  baseDir.mkdir
  def file(name : String) : File =
    new File(baseDir, name)
}

object FileAsset extends StoreDir("store.master") {
  private def relativeFile(asset : models.FileAsset) : File = {
    val i = asset.sha1
    new File(i.head.formatted("%02x"), new String(Hex(i.tail)))
  }
  private def masterFile(rel : File) : File =
    new File(baseDir, rel.getPath)
  private val fallbackDir =
    current.configuration.getString("store.fallback").map(new File(_))
  def file(asset : models.FileAsset) : File = {
    val r = relativeFile(asset)
    val f = masterFile(r)
    fallbackDir.filterNot(_ => f.exists).fold(f)(new File(_, r.getPath))
  }
  def store(asset : models.FileAsset, f : TemporaryFile) = {
    val d = masterFile(relativeFile(asset))
    if (d.exists) {
      if (org.apache.commons.io.FileUtils.contentEquals(f.file, d))
        f.clean
      else
        throw new FileAlreadyExistsException("hash collision")
    } else {
      d.getParentFile.mkdir
      f.moveTo(d)
      d.setReadOnly
    }
  }
  def read(f : models.BackedAsset) : StreamEnumerator =
    StreamEnumerator.fromFile(file(f.source))
}

private[store] object Segment extends StoreDir("store.cache") {
  protected def file(asset : models.FileAsset, ext : String) : File = {
    val i = asset.sha1
    new File(new File(baseDir, i.head.formatted("%02x")), new String(Hex(i.tail)) + ext)
  }

  /* Cache filenames use millisecond resolution */
  private def cacheEnabled = java.nio.file.Files.isWritable(baseDir.toPath)
  implicit val executionContext = site.context.foreground

  private def generate(file : File, gen : File => Unit, cache : Boolean = true) : Future[StreamEnumerator] =
    try {
      Future.successful(StreamEnumerator.fromFile(file))
    } catch { case _ : FileNotFoundException => Future {
      if (cache && cacheEnabled) {
        val d = file.getParentFile
        d.mkdir
        val t = File.createTempFile(file.getName, null, d)
        try {
          gen(t)
          if (!t.renameTo(file))
            throw new FileSystemException(file.getPath, t.getPath, "rename failed")
        } finally {
          t.delete /* safe due to createTempFile semantics */
        }
        StreamEnumerator.fromFile(file)
      } else {
        StreamEnumerator.fromFileGenerator(file.getName, gen)
      }
    } }

  private def frame(asset : models.FileAsset, offset : Option[Offset], size : Option[Int] = None, cache : Boolean = true) : Future[StreamEnumerator] = {
    val f = file(asset, offset.fold("")(_.millis.formatted(":%d")) + size.fold("")(_.formatted("@%d")))
    if (cache && cacheEnabled)
      generate(f, (f : File) => media.AV.frame(FileAsset.file(asset), offset, size.fold(-1)(_.toInt), f), cache)
    else Future {
      StreamEnumerator.fromData(media.AV.frame(FileAsset.file(asset), offset, size.fold(-1)(_.toInt)))
    }
  }

  private def segment(asset : models.FileAsset, section : Section, cache : Boolean = true) : Future[StreamEnumerator] = {
    val f = file(asset, ":%d-%d".format(section.lower.millis.toLong, section.upper.millis.toLong))
    generate(f, (f : File) => media.AV.segment(FileAsset.file(asset), section, f), cache)
  }

  private[store] def read(t : TimeseriesData, size : Option[Int]) : Future[StreamEnumerator] =
    t.section.singleton.fold(segment(t.source, t.section))(o => frame(t.source, Some(o), size))

  private[store] def resize(asset : BackedAsset, size : Int) : Future[StreamEnumerator] =
    frame(asset.source, None, Some(size))
}

object Asset {
  def read(o : BackedAsset, size : Option[Int] = None) : Future[StreamEnumerator] = o match {
    case t : TimeseriesData if !t.entire => Segment.read(t, size)
    case _ => size match {
      case Some(z) if o.format === AssetFormat.Image => Segment.resize(o, z)
      case _ => Future.successful(FileAsset.read(o))
    }
  }
  def timestamp(asset : models.BackedAsset) : Long =
    FileAsset.file(asset.source).lastModified
}
