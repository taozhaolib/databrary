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

  val videoFormat = "mov,mp4,m4a,3gp,3g2,mj2"
  val videoCodecs = Iterable("h264", "aac")

  /* These are referenced by native code so must match their use there */
  final class Error(msg : String, val err : Int) extends RuntimeException(msg)
  final class Probe(val format : String, _duration : Double, val streams : Array[String]) {
    def duration : Offset = _duration

    /** Test if this represents a video in primary format. */
    def isVideo : Boolean =
      format.equals(videoFormat) &&
      !streams.isEmpty &&
      streams.zip(videoCodecs).forall { case (s,c) => s.equals(c) }
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
    val r = new ProcessBuilder("ffmpeg", "-loglevel", "error", "-threads", "1", "-accurate_seek", 
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
