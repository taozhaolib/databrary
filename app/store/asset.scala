package store

import java.io.{File,InputStream,FileInputStream,FileNotFoundException}
import java.nio.{ByteBuffer,channels}
import java.nio.file.{StandardOpenOption,FileSystemException}
import scala.concurrent._
import scala.collection.mutable.{Map,Queue}
import play.api.Play.current
import play.api.libs.Files.TemporaryFile
import play.api.libs.concurrent.{Akka,Execution}
import play.api.libs.iteratee._
import dbrary.{Offset,Range}
import util._
import models._

trait StreamEnumerator extends Enumerator[Array[Byte]] {
  val size : Option[Long]
}
object StreamEnumerator {
  def apply(buf : Array[Byte]) = new StreamEnumerator {
    val size = Some(buf.length.toLong)
    def apply[A](it : Iteratee[Array[Byte], A]) : Future[Iteratee[Array[Byte], A]] = 
      Enumerator.apply(buf).apply[A](it)
  }
  def fromStream(input : InputStream, chunkSize : Int = 8192) = new StreamEnumerator {
    val size = None /* input.available */
    def apply[A](it : Iteratee[Array[Byte], A]) : Future[Iteratee[Array[Byte], A]] = 
      Enumerator.fromStream(input, chunkSize).apply[A](it)
  }
  def fromFile(file : File, chunkSize : Int = 8192) = new StreamEnumerator {
    val size = Some(file.length)
    private[this] val input = new FileInputStream(file)
    def apply[A](it : Iteratee[Array[Byte], A]) : Future[Iteratee[Array[Byte], A]] = 
      Enumerator.fromStream(input, chunkSize).apply[A](it)
  }
  def fromFileGenerator(name : String, gen : File => Unit) : StreamEnumerator = {
    val t = File.createTempFile(name, null)
    try {
      gen(t)
      StreamEnumerator.fromFile(t)
    } finally {
      t.delete
    }
  }
}

private[store] class StoreDir[Id <: IntId[_]](conf : String) {
  private[this] def getConfString(path : String) : String = {
    val c = current.configuration
    c.getString(path).getOrElse(throw c.globalError("Missing configuration for " + path))
  }
  protected lazy val base = new java.io.File(getConfString(conf))
  protected[store] def file(id : Id) : File = new File(base, id.unId.formatted("%010d"))
  protected def file(id : Id, ext : String) : File = new File(file(id).getPath + ext)
}

object FileAsset extends StoreDir[models.FileAsset.Id]("store.master") {
  def store(id : models.FileAsset.Id, f : TemporaryFile) =
    f.moveTo(file(id))
  def read(f : models.FileAsset) : StreamEnumerator =
    StreamEnumerator.fromFile(file(f.id))
}

object Segment extends StoreDir[models.Asset.Id]("store.cache") {
  private def cacheEnabled = base.exists
  implicit val executionContext : ExecutionContext = Akka.system.dispatchers.lookup("excerpt")

  private def generate(file : File, gen : File => Unit, cache : Boolean = true) : Future[StreamEnumerator] =
    try {
      Future.successful(StreamEnumerator.fromFile(file))
    } catch { case _ : FileNotFoundException => Future {
      if (cache && cacheEnabled) {
        val t = File.createTempFile(file.getName, null, file.getParentFile)
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

  private def genFrame(id : models.Timeseries.Id, offset : Offset, cache : Boolean = true) : Future[StreamEnumerator] =
    /* Using millisecond resolution: */
    if (cache && cacheEnabled)
      generate(file(id, offset.millis.toLong.formatted(":%d")), (f : File) => media.AV.frame(FileAsset.file(id), offset, f), cache)
    else Future {
      StreamEnumerator(media.AV.frame(FileAsset.file(id), offset))
    }

  private[store] def readFrame(t : models.Timeseries, offset : Offset) : Future[StreamEnumerator] =
    genFrame(t.id, offset, offset == 0)
  private[store] def readFrame(e : models.Clip, offset : Offset = 0) : Future[StreamEnumerator] =
    genFrame(e.sourceId, e.segment.lowerBound.get+offset, offset == 0)

  private[store] def read(e : models.Clip) : Future[StreamEnumerator] = 
    e.segment.singleton.fold {
      generate(file(e.id), (f : File) => media.AV.segment(FileAsset.file(e.sourceId), e.segment, f))
    } (genFrame(e.sourceId, _))
}

object Asset {
  def read(o : models.Asset) : Future[StreamEnumerator] = o match {
    case f : models.FileAsset => Future.successful(FileAsset.read(f))
    case e : models.Clip => Segment.read(e)
  }
  def readFrame(o : models.Asset, offset : Offset = 0) : Future[StreamEnumerator] = o match {
    case t : models.Timeseries if t.format == TimeseriesFormat.Video => Segment.readFrame(t, offset)
    case e : models.Clip => Segment.readFrame(e, offset)
  }
}
