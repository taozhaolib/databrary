package object store {
  def urlFile(u : java.net.URL) : Option[java.io.File] =
    if (u.getProtocol.equals("file"))
      Some(new java.io.File(u.getFile))
    else
      None

  private final val fileNamePad = "[\u0000-,/?\\\\]+".r
  def sanitizeFileName(s : String) : String =
    fileNamePad.replaceAllIn(s, "_")
}
