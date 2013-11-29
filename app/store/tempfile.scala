package store

import java.io.File
import play.api.libs.Files

final class TemporaryFileLinkOrCopy(file : File) extends Files.TemporaryFile(file) {
  override def clean() : Boolean = false
  private[this] def linkTo(to : File) {
    java.nio.file.Files.createLink(to.toPath, file.toPath)
  }
  private[this] def copyTo(to : File, replace : Boolean = false) {
    Files.copyFile(file, to, copyAttributes = false, replaceExisting = replace)
  }
  override def moveTo(to : File, replace : Boolean = false) {
    try {
      linkTo(to)
    } catch {
      case _ : UnsupportedOperationException => copyTo(to, replace)
    }
  }
}
object TemporaryFileLinkOrCopy {
  def apply(file : File) = new TemporaryFileLinkOrCopy(file)
}

