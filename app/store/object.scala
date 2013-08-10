package store

import java.io.{File,InputStream}
import java.nio._
import java.nio.file.StandardOpenOption
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
  def fromStream(input : InputStream, chunkSize : Int = 8192) = new StreamEnumerator {
    val size = None /* input.available */
    def apply[A](it : Iteratee[Array[Byte], A]) : Future[Iteratee[Array[Byte], A]] = 
      Enumerator.fromStream(input, chunkSize).apply[A](it)
  }
  def fromFile(file : File, chunkSize : Int = 8192) = new StreamEnumerator {
    val size = Some(file.length)
    def apply[A](it : Iteratee[Array[Byte], A]) : Future[Iteratee[Array[Byte], A]] = 
      Enumerator.fromFile(file, chunkSize).apply[A](it)
  }
}

private[store] class StoreDir[Id <: IntId[_]](conf : String) {
  private[this] def getConfString(path : String) : String = {
    val c = current.configuration
    c.getString(path).getOrElse(throw c.globalError("Missing configuration for " + path))
  }
  protected lazy val base = new java.io.File(getConfString(conf))
  protected[store] def file(id : Id) : File = new File(base, id.unId.formatted("%010d"))
  protected def dot(f : File, ext : String) : File = new File(f.getPath + "." + ext)
  protected def file(id : Id, ext : String) : File = dot(file(id), ext)
}

object FileObject extends StoreDir[models.FileObject.Id]("store.master") {
  def store(id : models.FileObject.Id, f : TemporaryFile) =
    f.moveTo(file(id))
  def read(f : models.FileObject) : StreamEnumerator = StreamEnumerator.fromFile(file(f.id))
}

object Excerpt extends StoreDir[models.Object.Id]("store.cache") {
  private def enabled = base.exists

  val executionContext : ExecutionContext = Akka.system.dispatchers.lookup("excerpt")

  type Key = (models.Object.Id, Boolean)
  private def file(id : Key) : File = if (id._2) file(id._1, "head") else file(id._1)

  private final class ActiveGenerator(val key : Key, gen : InputStream, val bufSize : Int = 8192) {
    val tmpFile = dot(file(key), "gen")
    val channel = channels.FileChannel.open(tmpFile.toPath,
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE, 
      StandardOpenOption.READ)
    private[this] var done = false
    /* how much we've written so far */
    private[this] var size : Long = 0
    /* all the promises waiting for more data */
    private[this] val waiting : Queue[Promise[Boolean]] = new Queue
    /* non-blocking poll */
    def waitAt(off : Long) : Future[Boolean] = synchronized {
      if (size > off)
        Future.successful(true)
      else if (done)
        Future.successful(false)
      else {
        val p = Promise[Boolean]()
        waiting.enqueue(p)
        p.future
      }
    }
    /* Write the excerpt to disk in a separate thread-pool */
    val run = Future {
      val buf = ByteBuffer.allocate(bufSize)
      def block() : Unit = {
        buf.clear
        val r = gen.read(buf.array, buf.arrayOffset, buf.capacity)
        if (r <= 0)
          return
        buf.limit(r)
        while (buf.hasRemaining) {
          val w = channel.write(buf, size)
          (synchronized {
            size += w
            waiting.dequeueAll(_ => true)
          }).foreach(_.success(true))
        }
        block
      }
      block
      (synchronized {
        done = true
        waiting.dequeueAll(_ => true)
      }).foreach(_.success(false))
      gen.close
      tmpFile.renameTo(file(key))
      active.synchronized {
        active -= key
      }
    } (executionContext)
    override def finalize = {
      channel.close
    }
  }
  private val active : Map[Key, ActiveGenerator] = Map[Key, ActiveGenerator]()

  private final class ActiveReader(gen : ActiveGenerator) extends StreamEnumerator {
    val size = None
    private[this] var off : Long = 0
    private[this] val buf = ByteBuffer.allocate(gen.bufSize)
    private[this] val parent = Enumerator.fromCallback1 { init =>
      gen.waitAt(off).map { rem =>
        if (rem) {
          buf.clear
          val r = gen.channel.read(buf, off)
          val b = new Array[Byte](r)
          buf.flip
          buf.get(b)
          off += r
          Some(b)
        } else
          None
      } (Execution.defaultContext)
    }
    def apply[A](it : Iteratee[Array[Byte], A]) : Future[Iteratee[Array[Byte], A]] = 
      parent.apply[A](it)
  }

  private def cached(key : Key, gen : => InputStream) : StreamEnumerator =
    if (!enabled)
      StreamEnumerator.fromStream(gen)
    else {
      val f = file(key)
      active.synchronized {
        if (f.exists)
          StreamEnumerator.fromFile(f)
        else
          new ActiveReader(active.getOrElse(key, new ActiveGenerator(key, gen)))
      }
    }

  private def readFrame(key : Key, src : models.TimeseriesObject.Id, offset : Interval) : StreamEnumerator =
    cached(key, media.AV.extractFrame(FileObject.file(src), offset))
  def read(e : models.Excerpt) : StreamEnumerator = 
    e.duration.fold(readFrame((e.id, false), e.sourceId, e.offset)) { len =>
      cached((e.id, false), ???)
    }
  def readHead(e : models.Excerpt) : StreamEnumerator =
    readFrame((e.id, e.duration.nonEmpty), e.sourceId, e.offset)
  def readHead(t : models.TimeseriesObject) : StreamEnumerator =
    readFrame((t.id, true), t.id, Interval(0))
}

object Object {
  def read(o : models.Object) : StreamEnumerator = o match {
    case f : models.FileObject => FileObject.read(f)
    case e : models.Excerpt => Excerpt.read(e)
  }
  def readHead(o : models.Object) : StreamEnumerator = o match {
    case t : models.TimeseriesObject => Excerpt.readHead(t)
    case e : models.Excerpt => Excerpt.readHead(e)
  }
}
