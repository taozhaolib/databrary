package store

import java.io.File
import play.api.libs.Files

final class TemporaryFileLinkOrCopy(file : File) extends Files.TemporaryFile(file) {
  override def clean() : Boolean = false
  private[this] def linkTo(to : File) {
    java.nio.file.Files.createLink(to.toPath, file.toPath)
  }
  private[this] def copyTo(to : File, replace : Boolean = false) {
    if (replace)
      java.nio.file.Files.copy(file.toPath, to.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    else
      java.nio.file.Files.copy(file.toPath, to.toPath)
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

