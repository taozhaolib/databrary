package site

/** Utility that creates [[scala.Option]]s out of values. */
object maybe {
  def guard[A](g : Boolean, v : => A) : Option[A] =
    if (g) Some(v) else None

  /** A more concise version of the common `Some(_).filter(_)` idiom.
    * @return Some(a) if f(a), None otherwise
    */
  def apply[A](a : A, f : A => Boolean) : Option[A] =
    Some(a).filter(f)

  /** Eliminate a specific value, like SQL's `NULLIF`.
    * @return Some(a) unless a == n
    */
  def apply[A](a : A, n : A) : Option[A] =
    maybe(a, (_ : A) != n)
  /** Default empty value for Strings. */
  def apply(s : String, f : String => Boolean = !_.isEmpty) =
    Some(s).filter(f)
}
