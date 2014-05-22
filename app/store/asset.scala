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
  private[this] def getConfString(path : String) : String = {
    val c = current.configuration
    c.getString(path).getOrElse(throw c.globalError("Missing configuration for " + path))
  }
  protected lazy val base = new File(getConfString(conf))
}

object Stage extends StoreDir("store.stage") {
  def file(f : File) : File =
    if (f.isAbsolute) f else new File(base, f.getPath)
  def file(s : String) : File =
    file(new File(s))
  def path(f : File) : String =
    f.getPath.stripPrefix(base.getPath + '/')

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

object FileAsset extends StoreDir("store.master") {
  protected[store] def file(asset : models.Asset) : File = {
    val i = asset.sha1
    new File(new File(base, i.head.formatted("%02x")), new String(Hex(i.tail)))
  }
  def store(asset : models.Asset, f : TemporaryFile) = {
    val d = file(asset)
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
  protected def file(id : models.Asset.Id, ext : String) : File = {
    val i = id.unId
    new File(new File(base, (i & 0xff).formatted("%02x")), (i >> 8).formatted("%06x:") + ext)
  }

  /* Cache filenames use millisecond resolution */
  private def cacheEnabled = base.exists
  implicit val executionContext = site.context.process

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

  private def genFrame(asset : models.Asset, offset : Offset, cache : Boolean = true) : Future[StreamEnumerator] = {
    val f = file(asset.id, offset.millis.toLong.formatted("%d"))
    if (cache && cacheEnabled)
      generate(f, (f : File) => media.AV.frame(FileAsset.file(asset), offset, f), cache)
    else Future {
      StreamEnumerator.fromData(media.AV.frame(FileAsset.file(asset), offset))
    }
  }

  private def genSegment(asset : models.Asset, section : Section, cache : Boolean = true) : Future[StreamEnumerator] = {
    val f = file(asset.id, "%d-%d".format(section.lower.millis.toLong, section.upper.millis.toLong))
    generate(f, (f : File) => media.AV.segment(FileAsset.file(asset), section, f), cache)
  }

  private[store] def readFrame(t : TimeseriesData, offset : Offset) : Future[StreamEnumerator] =
    genFrame(t.source, t.section.lower+offset)

  private[store] def read(t : TimeseriesData) : Future[StreamEnumerator] = 
    t.section.singleton.fold(genSegment(t.source, t.section))(genFrame(t.source, _))
}

object Asset {
  def read(o : BackedAsset) : Future[StreamEnumerator] = o match {
    case t : TimeseriesData if !t.entire => Segment.read(t)
    case _ => Future.successful(FileAsset.read(o))
  }
  def timestamp(asset : models.BackedAsset) : Long =
    FileAsset.file(asset.source).lastModified
}
