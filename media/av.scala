package media

object AV {
  loadLibrary("av")

  private def loadLibrary(name : String) = {
    val lib = System.mapLibraryName(name)
    /* We dump a copy of the library in the current directory; this may not be the best choice */
    val file = new java.io.File(lib).getAbsoluteFile
    /* always copy in case of updates */
    if (true || !file.exists) {
      val loader = this.getClass.getClassLoader
      val in = loader.getResourceAsStream(lib)
      val out = new java.io.FileOutputStream(file)
      org.apache.commons.io.IOUtils.copy(in, out)
      in.close
      out.close
    }
    System.load(file.getPath)
    /* In development, this can throw 
     *  e : java.lang.UnsatisfiedLinkError if e.getMessage.endsWith("already loaded in another classloader")
     * which seems to break things all around.
     */
  }

  /* These are referenced by native code so must match their use there */
  final case class AVError(msg : String, err : Int) extends RuntimeException(msg)
  final case class AVProbe(streams : Int)

  @native def probe(file : String) : AVProbe
}
