package media

import java.lang.{ProcessBuilder}
import java.io.File
import dbrary.{Offset,Section}

object AV {
  init

  private val videoFormat = "mov,mp4,m4a,3gp,3g2,mj2"
  private val videoCodecs = Seq("h264", "aac")

  /* These are referenced by native code so must match their use there */
  final class Error(val file : String, msg : String, val err : Int) extends RuntimeException(msg)
  final class Probe(val format : String, _duration : Double, val streams : Array[String]) {
    def duration : Option[Offset] =
      if (_duration > 0) Some(Offset.ofSeconds(_duration)) else None

    /** Test if this represents a video in primary format. */
    def isVideo : Boolean =
      format.equals(videoFormat) &&
      !streams.isEmpty &&
      streams.zip(videoCodecs).forall { case (s,c) => s.equals(c) }
  }

  @native def _probe(file : String) : Probe
  @native def _frame(infile : String, offset : Double, outfile : String, width : Int = -1, height : Int = -1) : Array[Byte]
  def probe(file : File) : Probe = _probe(file.getPath)
  def frame(infile : File, offset : Option[Offset], size : Int) : Array[Byte] =
    _frame(infile.getPath, offset.fold(Double.NaN)(_.seconds), null, size, -1)
  def frame(infile : File, offset : Option[Offset], size : Int, outfile : File) : Unit =
    _frame(infile.getPath, offset.fold(Double.NaN)(_.seconds), outfile.getPath, size, -1)
  def segment(infile : File, section : Section, outfile : File) : Unit = {
    /* XXX this rounds outwards to keyframes and does other strange things with timing */
    val r = new ProcessBuilder("ffmpeg", "-y", "-loglevel", "error", "-threads", "1", "-accurate_seek", 
      "-ss", section.lower.seconds.toString, 
      "-i", infile.getPath, 
      "-t", (section.upper - section.lower).seconds.toString,
      "-codec", "copy",
      "-f", "mp4",
      outfile.getPath).
      inheritIO.redirectInput(new File("/dev/null")).start.waitFor
    if (r != 0)
      throw new Error(infile.getPath, "extractSegment failed", r)
  }
}
