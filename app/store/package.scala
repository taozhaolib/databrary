package object store {
  def urlFile(u : java.net.URL) : Option[java.io.File] =
    if (u.getProtocol.equals("file"))
      Some(new java.io.File(u.getFile))
    else
      None
}
