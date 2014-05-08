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
      parent.through(StreamEnumerator.range(start, stop)).apply[A](it)
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
      Enumerator.fromStream(input, chunkSize)(play.api.libs.concurrent.Execution.defaultContext).apply[A](it)
    override def range(start : Long = 0, end : Long = size - 1) = new Range(start, end) {
      input.skip(start)
      override def apply[A](it : Iteratee[Data, A]) : Future[Iteratee[Data, A]] = {
        parent.through(StreamEnumerator.range(0, stop-start)).apply[A](it)
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
  /** Composition of Enumeratee.take and Enumeratee.drop with Longs. */
  def range(start : Long, stop : Long) : Enumeratee[Data, Data] = new Enumeratee.CheckDone[Data, Data] {
    private def next[A](pos : Long, i : Iteratee[Data, A]) =
      new Enumeratee.CheckDone[Data, Data] { def continue[A](k: K[Data, A]) = Cont(step(pos)(k)) } &> i
    def step[A](pos : Long)(k : K[Data, A]) : K[Data, Iteratee[Data, A]] = {
      case in @ Input.El(a) if pos + a.length <= start => Cont(step(pos+a.length)(k))
      case in @ Input.El(a) if pos < start && pos + a.length > stop => Done(k(Input.El(a.slice((start-pos).toInt, (stop-pos).toInt))), Input.Empty)
      case in @ Input.El(a) if pos < start => next(pos+a.length, k(Input.El(a.drop((start-pos).toInt))))
      case in @ Input.El(a) if pos + a.length <= stop => next(pos+a.length, k(in))
      case in @ Input.El(a) if pos < stop => Done(k(Input.El(a.take((stop-pos).toInt))), Input.Empty)
      case in @ Input.Empty if pos < start => Cont(step(pos)(k))
      case in @ Input.Empty if pos < stop => next(pos, k(in))
      case in => Done(Cont(k), in)
    }
    def continue[A](k: K[Data, A]) = Cont(step(0)(k))
  }
}
