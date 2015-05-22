package macros

import scala.concurrent.{Future,ExecutionContext}

trait OptionVar[T] {
  def peek : Option[T]
  /** Unsafely retrieve the already computed value. */
  def get : T
  /** Change the value, discarding any current computation. */
  def set(v : T) : Unit
  // def map[A](f : T => A) : OptionVar[A]
  def clear() : Unit
}

class LazyVar[T](init : => T) {
  final protected var value : Option[T] = None
  /** (Compute and) return the current value. */
  final def apply() : T = synchronized(value.getOrElse(update(init)))
  final def update(v : T) : T = {
    value = Some(v)
    v
  }
  final def clear() {
    value = None
  }
}

/** A variable with lazy semantics, what "lazy var" should be. */
final class LazyOptionVar[T](init : => T) extends LazyVar[T](init) with OptionVar[T] {
  def peek : Option[T] = value
  def get : T = value.get
  def set(v : T) = update(v)
  def map[A](f : T => A) : LazyOptionVar[A] = {
    val v = new LazyOptionVar[A](f(apply()))
    v.value = value.map(f)
    v
  }
}

object LazyVar {
  def apply[T](init : => T) = new LazyOptionVar[T](init)
}

/** A LazyVar[Future[T]] with some extra functionality. */
final class FutureVar[T](init : => Future[T]) extends LazyVar[Future[T]](init) with OptionVar[T] {
  def peek : Option[T] = value.flatMap(_.value.map(_.get))
  def get : T = value.get.value.get.get
  def set(v : T) = update(Future.successful(v))
  def map[A](f : T => A)(implicit ctx : ExecutionContext) : FutureVar[A] = {
    val v = new FutureVar[A](apply().map(f))
    v.value = value.map(x => x.value.fold(x.map(f))(v => v.map(f).toFuture))
    v
  }
}

object FutureVar {
  def apply[T](init : => Future[T]) = new FutureVar[T](init)
}
