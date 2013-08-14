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
import dbrary.Interval
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

object FileObject extends StoreDir[models.FileObject.Id]("store.master") {
  def store(id : models.FileObject.Id, f : TemporaryFile) =
    f.moveTo(file(id))
  def read(f : models.FileObject) : StreamEnumerator =
    StreamEnumerator.fromFile(file(f.id))
}

object Excerpt extends StoreDir[models.Object.Id]("store.cache") {
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

  private def genFrame(id : models.TimeseriesObject.Id, offset : Interval, cache : Boolean = true) : Future[StreamEnumerator] =
    /* Using millisecond resolution: */
    if (cache && cacheEnabled)
      generate(file(id, offset.millis.toLong.formatted(":%d")), (f : File) => media.AV.frame(FileObject.file(id), offset, f), cache)
    else Future {
      StreamEnumerator(media.AV.frame(FileObject.file(id), offset))
    }

  private[store] def readFrame(t : models.TimeseriesObject, offset : Interval) : Future[StreamEnumerator] =
    genFrame(t.id, offset, offset == Interval(0))
  private[store] def readFrame(e : models.Excerpt, offset : Interval = Interval(0)) : Future[StreamEnumerator] =
    genFrame(e.sourceId, e.offset+offset, offset == Interval(0))

  private[store] def read(e : models.Excerpt) : Future[StreamEnumerator] = 
    e.duration.fold(readFrame(e)) { len =>
      generate(file(e.id), (f : File) => media.AV.segment(FileObject.file(e.sourceId), e.offset, len, f))
    }
}

object Object {
  def read(o : models.Object) : Future[StreamEnumerator] = o match {
    case f : models.FileObject => Future.successful(FileObject.read(f))
    case e : models.Excerpt => Excerpt.read(e)
  }
  def readFrame(o : models.Object, offset : Interval = Interval(0)) : Future[StreamEnumerator] = o match {
    case t : models.TimeseriesObject if t.isVideo => Excerpt.readFrame(t, offset)
    case e : models.Excerpt => Excerpt.readFrame(e, offset)
  }
}
