package store

import play.api.Play.current
import play.api.libs.Files.TemporaryFile
import util._

object Object {
  private[this] def getConfString(path : String) : String = {
    val c = current.configuration
    c.getString(path).getOrElse(throw c.globalError("Missing configuration for " + path))
  }
  private[this] lazy val base = new java.io.File(getConfString("store.dir"))

  def file(id : models.FileObject.Id) = new java.io.File(base, id.unId.formatted("%010d"))
  def store(id : models.FileObject.Id, f : TemporaryFile) =
    f.moveTo(file(id))
}
