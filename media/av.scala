package media

import java.lang.{ProcessBuilder}
import java.io.{InputStream,File,FileOutputStream}
import dbrary.{Offset,Range}

object AV {
  loadLibrary("av")

  private def loadLibrary(name : String) = {
    val lib = System.mapLibraryName(name)
    val dot = lib.lastIndexOf('.')
    val file = File.createTempFile(lib.substring(0, dot), lib.substring(dot, lib.length))
    val loader = this.getClass.getClassLoader
    val in = loader.getResourceAsStream(lib)
    val out = new FileOutputStream(file)
    org.apache.commons.io.IOUtils.copy(in, out)
    in.close
    out.close
    System.load(file.getPath)
    file.delete/*OnExit -- may be necessary on some platforms? */
  }

  /* These are referenced by native code so must match their use there */
  final case class Error(msg : String, err : Int) extends RuntimeException(msg)
  final case class Probe(format : String, _duration : Double) {
    def duration : Offset = _duration
  }

  @native def _probe(file : String) : Probe
  @native def _frame(infile : String, offset : Double, outfile : String) : Array[Byte]
  def probe(file : File) : Probe = _probe(file.getPath)
  def frame(infile : File, offset : Offset) : Array[Byte] =
    _frame(infile.getPath, offset.seconds, null)
  def frame(infile : File, offset : Offset, outfile : File) : Unit =
    _frame(infile.getPath, offset.seconds, outfile.getPath)
  def segment(infile : File, segment : Range[Offset], outfile : File) : Unit = {
    /* XXX this rounds outwards to keyframes and does other strange things with timing */
    val r = new ProcessBuilder("ffmpeg", "-loglevel", "error", "-accurate_seek", 
      "-ss", segment.lowerBound.get.seconds.toString, 
      "-to", segment.upperBound.get.seconds.toString, 
      "-i", infile.getPath, 
      "-codec", "copy", 
      outfile.getPath).
      inheritIO.redirectInput(new File("/dev/null")).start.waitFor
    if (r != 0)
      throw new Error("extractSegment failed", r)
  }
}
