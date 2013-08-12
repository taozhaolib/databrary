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
  def fromFileGenerator(name : String = "gen", gen : File => Unit) : StreamEnumerator = {
    val t = File.createTempFile(name, "gen")
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
  protected def dot(f : File, ext : String) : File = new File(f.getPath + "." + ext)
  protected def file(id : Id, ext : String) : File = dot(file(id), ext)
}

object FileObject extends StoreDir[models.FileObject.Id]("store.master") {
  def store(id : models.FileObject.Id, f : TemporaryFile) =
    f.moveTo(file(id))
  def read(f : models.FileObject) : StreamEnumerator =
    StreamEnumerator.fromFile(file(f.id))
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
        try {
          StreamEnumerator.fromFile(f)
        } catch { case _ : FileNotFoundException =>
          new ActiveReader(active.getOrElse(key, new ActiveGenerator(key, gen)))
        }
      }
    }

  private def cached(file : File, gen : File => Unit) : Future[StreamEnumerator] = {
    implicit val ec = executionContext
    if (!enabled) Future {
      StreamEnumerator.fromFileGenerator(file.getName, gen)
    } else try {
      Future.successful(StreamEnumerator.fromFile(file))
    } catch { case _ : FileNotFoundException => Future {
      val t = File.createTempFile(file.getName, null, file.getParentFile)
      try {
        gen(t)
        if (!t.renameTo(file))
          throw new FileSystemException(file.getPath, t.getPath, "rename failed")
      } finally {
        t.delete /* safe due to createTempFile semantics */
      }
      StreamEnumerator.fromFile(file)
    } }
  }

  private[store] def read(e : models.Excerpt) : Future[StreamEnumerator] = 
    cached(file(e.id), (f : File) => e.duration.fold(
      media.AV.frame(FileObject.file(e.sourceId), e.offset, f))(
      len => media.AV.extractSegment(FileObject.file(e.sourceId), e.offset, len, f)))

  private def readFrame(key : Key, src : models.TimeseriesObject.Id, offset : Interval) : Future[StreamEnumerator] =
    cached(file(key), (f : File) => media.AV.frame(FileObject.file(src), offset, f))
  private[store] def readHead(e : models.Excerpt) : Future[StreamEnumerator] =
    readFrame((e.id, e.duration.nonEmpty), e.sourceId, e.offset)
  private[store] def readHead(t : models.TimeseriesObject) : Future[StreamEnumerator] =
    readFrame((t.id, true), t.id, Interval(0))

  private[store] def readFrame(t : models.TimeseriesObject, offset : Interval) : Future[StreamEnumerator] =
    Future { StreamEnumerator.fromFileGenerator(t.id.unId.toString, (f : File) =>
      media.AV.frame(FileObject.file(t.id), offset, f)) } (executionContext)
  private[store] def readFrame(e : models.Excerpt, offset : Interval) : Future[StreamEnumerator] =
    readFrame(e.source, e.offset+offset)
}

object Object {
  def read(o : models.Object) : Future[StreamEnumerator] = o match {
    case f : models.FileObject => Future.successful(FileObject.read(f))
    case e : models.Excerpt => Excerpt.read(e)
  }
  def readHead(o : models.Object) : Future[StreamEnumerator] = o match {
    case t : models.TimeseriesObject if t.isVideo => Excerpt.readHead(t)
    case e : models.Excerpt => Excerpt.readHead(e)
  }
  def readFrame(o : models.Object, offset : Interval) : Future[StreamEnumerator] = o match {
    case t : models.TimeseriesObject if t.isVideo => Excerpt.readFrame(t, offset)
    case e : models.Excerpt => Excerpt.readFrame(e, offset)
  }
}
