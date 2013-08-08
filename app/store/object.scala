package store

import java.io._
import play.api.Play.current
import play.api.libs.Files.TemporaryFile
import util._
import models._

private[store] class StoreDir[Id <: IntId[_]](conf : String) {
  private[this] def getConfString(path : String) : String = {
    val c = current.configuration
    c.getString(path).getOrElse(throw c.globalError("Missing configuration for " + path))
  }
  private[this] lazy val base = new java.io.File(getConfString(conf))
  def file(id : Id) = new File(base, id.unId.formatted("%010d"))
}

object Object extends StoreDir[FileObject.Id]("store.master") {
  def store(id : FileObject.Id, f : TemporaryFile) =
    f.moveTo(file(id))
}

object Excerpt extends StoreDir[models.Excerpt.Id]("store.cache") {
  /* This will all need to be done properly with Akka actors and asynchronous IO */

  /*
  private final class ActiveGenerator(file : String, gen : => InputStream) {
    var started = false;
  }
  private val active : HashMap[Int, ActiveGenerator] = new HashMap[Int, ActiveGenerator]

  private final class ActiveReader(gen : ActiveGenerator) extends InputStream {
  }
  */

  def cached(id : models.Excerpt.Id, gen : => InputStream) : InputStream = gen /*{
    val f = file(id)
    active.synchronized {
      try {
        new FileInputStream(f)
      } catch {
        case FileNotFoundException =>
          new ActiveReader(active.getOrElse(id.unId, new ActiveGenerator(f, gen)))
      }
    }
  } */
}
