package site

/** A variable with a default value depending on some state.
  * @tparam T the type of the variable/value
  * @tparam S the type of state required to access/compute the value.
  * @param init function to compute the initial default value from state
  */
class CachedVal[T, S](init : S => T) {
  protected var x : Option[T] = None
  /** (Compute and) return the current value from a state. */
  final def apply(state : S) : T = synchronized(x.getOrElse(update(init(state))))
  /** Change the value, discarding any current computation or future state. */
  final def update(v : T) : T = {
    x = Some(v)
    v
  }
  final def isEmpty : Boolean = x.isEmpty
}
object CachedVal {
  import scala.language.implicitConversions
  /** Constructor. */
  def apply[T, S](init : S => T) = new CachedVal[T,S](init)
  /** Evaluate the cached value from an implicit state. */
  implicit def implicitGetCached[T, S](x : CachedVal[T, S])(implicit s : S) : T = x(s)
}
