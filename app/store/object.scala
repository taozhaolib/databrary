package store

import java.io.{File,InputStream}
import java.nio._
import java.nio.file.StandardOpenOption
import scala.concurrent._
import scala.collection.mutable.{HashMap,Queue}
import play.api.Play.current
import play.api.libs.Files.TemporaryFile
import play.api.libs.concurrent.{Akka,Execution}
import play.api.libs.iteratee._
import util._
import models._

private[store] class StoreDir[Id <: IntId[_]](conf : String) {
  private[this] def getConfString(path : String) : String = {
    val c = current.configuration
    c.getString(path).getOrElse(throw c.globalError("Missing configuration for " + path))
  }
  protected lazy val base = new java.io.File(getConfString(conf))
  def file(id : Id) = new File(base, id.unId.formatted("%010d"))
}

object Object extends StoreDir[FileObject.Id]("store.master") {
  def store(id : FileObject.Id, f : TemporaryFile) =
    f.moveTo(file(id))
}

object Excerpt extends StoreDir[models.Excerpt.Id]("store.cache") {
  private def enabled = base.exists

  val executionContext : ExecutionContext = Akka.system.dispatchers.lookup("excerpt")

  private final class ActiveGenerator(val key : Int, val file : File, gen : => InputStream, val bufSize : Int = 8192) {
    val tmpFile = new File(file.getPath + ".gen")
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
      tmpFile.renameTo(file)
      active.synchronized {
        active -= key
      }
    } (executionContext)
    override def finalize = {
      channel.close
    }
  }
  private val active : HashMap[Int, ActiveGenerator] = new HashMap[Int, ActiveGenerator]

  private def activeReader(gen : ActiveGenerator) = {
    var off : Long = 0
    val buf = ByteBuffer.allocate(gen.bufSize)
    Enumerator.fromCallback1 { init =>
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
  }

  def cached(id : models.Excerpt.Id, gen : => InputStream) : Enumerator[Array[Byte]] =
    if (!enabled)
      Enumerator.fromStream(gen)
    else {
      val f = file(id)
      active.synchronized {
        try {
          Enumerator.fromFile(f)
        } catch {
          case _ : java.io.FileNotFoundException =>
            activeReader(active.getOrElse(id.unId, new ActiveGenerator(id.unId, f, gen)))
        }
      }
    }
}
