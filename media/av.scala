package media

object AV {
  loadLibrary("av")

  private def loadLibrary(name : String) = {
    val lib = System.mapLibraryName(name)
    val dot = lib.lastIndexOf('.')
    val file = java.io.File.createTempFile(lib.substring(0, dot), lib.substring(dot, lib.length))
    val loader = this.getClass.getClassLoader
    val in = loader.getResourceAsStream(lib)
    val out = new java.io.FileOutputStream(file)
    org.apache.commons.io.IOUtils.copy(in, out)
    in.close
    out.close
    System.load(file.getPath)
    file.delete/*OnExit -- may be necessary on some platforms? */
  }

  /* These are referenced by native code so must match their use there */
  final case class Error(msg : String, err : Int) extends RuntimeException(msg)
  final case class Probe(streams : Int)

  @native def probe(file : String) : Probe
}
