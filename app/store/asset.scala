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

sealed trait StreamEnumerator extends Enumerator[Array[Byte]] {
  parent =>
  val size : Long
  def range(start : Long = 0, end : Long = size - 1) : StreamEnumerator = new StreamEnumerator {
    val size = end - start + 1
    def apply[A](it : Iteratee[Array[Byte], A]) : Future[Iteratee[Array[Byte], A]] =
      parent.through(StreamEnumerator.range(start, end+1)).apply[A](it)
  }
}
object StreamEnumerator {
  type Data = Array[Byte]
  def fromData(buf : Data) = new StreamEnumerator {
    val size = buf.length.toLong
    def apply[A](it : Iteratee[Data, A]) : Future[Iteratee[Data, A]] = 
      Enumerator.apply(buf).apply[A](it)
  }
  def fromFile(file : File, chunkSize : Int = 32768) = new StreamEnumerator {
    val size = file.length
    private[this] val input = new FileInputStream(file)
    def apply[A](it : Iteratee[Data, A]) : Future[Iteratee[Data, A]] = 
      Enumerator.fromStream(input, chunkSize).apply[A](it)
  }
  def fromFileGenerator(name : String, gen : File => Unit) : StreamEnumerator = {
    val t = File.createTempFile(name, null)
    try {
      gen(t)
      fromFile(t)
    } finally {
      t.delete
    }
  }
  /** Composition of Enumeratee.take and Enumeratee.drop with Longs. */
  def range(start : Long, stop : Long) : Enumeratee[Data, Data] = new Enumeratee.CheckDone[Data, Data] {
    private def next[A](pos : Long, i : Iteratee[Data, A]) =
      new Enumeratee.CheckDone[Data, Data] { def continue[A](k: K[Data, A]) = Cont(step(pos)(k)) } &> i
    def step[A](pos : Long)(k : K[Data, A]) : K[Data, Iteratee[Data, A]] = {
      case in @ Input.El(a) if pos + a.length <= start => Cont(step(pos+a.length)(k))
      case in @ Input.El(a) if pos < start && pos + a.length > stop => Done(k(Input.El(a.slice((start-pos).toInt, (stop-pos).toInt))), Input.Empty)
      case in @ Input.El(a) if pos < start => next(pos+a.length, k(Input.El(a.drop((start-pos).toInt))))
      case in @ Input.El(a) if pos + a.length <= stop => next(pos+a.length, k(in))
      case in @ Input.El(a) if pos < stop => Done(k(Input.El(a.take((stop-pos).toInt))), Input.Empty)
      case in @ Input.Empty if pos < start => Cont(step(pos)(k))
      case in @ Input.Empty if pos < stop => next(pos, k(in))
      case in => Done(Cont(k), in)
    }
    def continue[A](k: K[Data, A]) = Cont(step(0)(k))
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
  def read(f : models.BackedAsset) : StreamEnumerator =
    StreamEnumerator.fromFile(file(f.sourceId))
}

private[store] object Segment extends StoreDir[models.Asset.Id]("store.cache") {
  /* Cache filenames use millisecond resolution */
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

  private def genFrame(id : models.Timeseries.Id, offset : Offset, cache : Boolean = true) : Future[StreamEnumerator] = {
    val f = file(id, offset.millis.toLong.formatted(":%d"))
    if (cache && cacheEnabled)
      generate(f, (f : File) => media.AV.frame(FileAsset.file(id), offset, f), cache)
    else Future {
      StreamEnumerator.fromData(media.AV.frame(FileAsset.file(id), offset))
    }
  }

  private def genSegment(id : models.Timeseries.Id, segment : Range[Offset], cache : Boolean = true) : Future[StreamEnumerator] = {
    val f = file(id, ":%d-%d".format(segment.lowerBound.get.millis.toLong, segment.upperBound.get.millis.toLong))
    generate(f, (f : File) => media.AV.segment(FileAsset.file(id), segment, f), cache)
  }

  private[store] def readFrame(t : TimeseriesData, offset : Offset) : Future[StreamEnumerator] =
    genFrame(t.sourceId, t.segment.lowerBound.get+offset, offset.seconds == 0)

  private[store] def read(t : TimeseriesData) : Future[StreamEnumerator] = 
    t.segment.singleton.fold(genSegment(t.sourceId, t.segment))(genFrame(t.sourceId, _))
}

object Asset {
  def read(o : BackedAsset) : Future[StreamEnumerator] = o match {
    case t : TimeseriesData if !t.entire => Segment.read(t)
    case _ => Future.successful(FileAsset.read(o))
  }
  def readFrame(o : TimeseriesData, offset : Offset = 0) : Future[StreamEnumerator] =
    Segment.readFrame(o, offset)
}
