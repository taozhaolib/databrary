package media

import java.lang._
import java.io._
import dbrary.Interval

/* This should be redone somehow using asynchronous IO, though I have not found
 * any library that supports this on pipes (netty comes the closest). */
final class ProcessInputStream(process : Process) extends InputStream {
  private[this] val input = process.getInputStream
  override def close = {
    input.close
    process.destroy
  }
  private[this] def check(r : Int) = {
    if (r == -1)
      process.waitFor
    r
  }
  override def read : Int = check(input.read)
  override def read(b : Array[scala.Byte]) : Int = check(input.read(b))
  override def read(b : Array[scala.Byte], off : Int, len : Int) : Int = check(input.read(b, off, len))
}
object ProcessInputStream {
  import scala.collection.JavaConverters._
  def apply(cmd : String*) =
    new ProcessInputStream(new ProcessBuilder(cmd.asJava).redirectInput(new File("/dev/null")).redirectError(ProcessBuilder.Redirect.INHERIT).start)
}

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
  final case class Probe(format : String, _duration : scala.Double) {
    def duration : Interval = Interval(_duration)
  }

  @native def _probe(file : String) : Probe
  def probe(file : File) : Probe = _probe(file.getPath)
  def extractFrame(file : File, offset : Interval) : InputStream =
    ProcessInputStream("ffmpeg", "-loglevel", "error", "-accurate_seek", "-ss", offset.seconds.toString, "-i", file.getPath, "-f:v", "image2pipe", "-frames:v", "1", "-")
  def extractSegment(file : File, offset : Interval, duration : Interval) : InputStream =
    /* XXX this rounds outwards to keyframes and does other strange things with timing; also it doesn't work at all because mp4 can't output to pipes */
    ProcessInputStream("ffmpeg", "-loglevel", "error", "-accurate_seek", "-ss", offset.seconds.toString, "-t", duration.seconds.toString, "-i", file.getPath, "-codec", "copy", "-")
}
