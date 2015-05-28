package store

import java.io.{File,FileInputStream}
import play.api.libs.iteratee._
import scala.concurrent.Future
import macros._
import site._

sealed trait StreamEnumerator extends Enumerator[Array[Byte]] {
  parent =>
  val size : Long
  protected class Range(protected val start : Long, protected val end : Long) extends StreamEnumerator {
    protected val stop = end + 1
    val size = stop - start
    def apply[A](it : Iteratee[Array[Byte], A]) : Future[Iteratee[Array[Byte], A]] =
      parent.through(org.databrary.iteratee.Enumeratee.range(start, stop)).apply[A](it)
  }
  /** Produce a new Enumerator reflecting a segment of the current one.
    * This may invalidate the current enumerator. */
  def range(start : Long = 0, end : Long = size - 1) = new Range(start, end)
}
object StreamEnumerator {
  type Data = Array[Byte]
  def fromData(buf : Data) = new StreamEnumerator {
    val size = buf.length.toLong
    def apply[A](it : Iteratee[Data, A]) : Future[Iteratee[Data, A]] =
      Enumerator.apply(buf).apply[A](it)
    override def range(start : Long = 0, end : Long = size - 1) = new Range(start, end) {
      override def apply[A](it : Iteratee[Data, A]) : Future[Iteratee[Data, A]] =
        Enumerator.apply(buf.slice(start.toInt, stop.toInt)).apply[A](it)
    }
  }
  def fromFile(file : File, chunkSize : Int = 32768) = new StreamEnumerator {
    parent =>
    val size = file.length
    private[this] val input = new FileInputStream(file)
    def apply[A](it : Iteratee[Data, A]) : Future[Iteratee[Data, A]] =
      Enumerator.fromStream(input, chunkSize)(site.context.foreground).apply[A](it)
    override def range(start : Long = 0, end : Long = size - 1) = new Range(start, end) {
      val pos = input.skip(start)
      override def apply[A](it : Iteratee[Data, A]) : Future[Iteratee[Data, A]] = {
        parent.through(org.databrary.iteratee.Enumeratee.range(start-pos, stop-pos)).apply[A](it)
      }
    }
  }
  def fromFileGenerator(name : String, gen : File => Unit) : StreamEnumerator = {
    val t = File.createTempFile(name, null)
    try {
      gen(t)
      fromFile(t)
    } finally {
      t.delete
    }
  }
}
