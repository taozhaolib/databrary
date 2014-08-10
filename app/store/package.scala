package object store {
  def urlFile(u : java.net.URL) : Option[java.io.File] =
    if (u.getProtocol.equals("file"))
      Some(new java.io.File(u.getFile))
    else
      None

  private final val fileNamePad = "[\u0000-,/?\\\\]+".r
  def fileName(s : String*) : String =
    fileNamePad.replaceAllIn(s.mkString("-"), "_")

  def truncate(s : String, max : Int = 32) : String =
    s.take(macros.Maybe(s.lastIndexOf(' ', 32)).orElse(32))
}
