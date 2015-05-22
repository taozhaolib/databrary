package macros

import scala.language.higherKinds

/** Boiler-plate to produce heterogenous lists where individual elements are processed using a view.
  * @tparam V the view bound: an implicit V[A] trait for individual arguments
  * @tparam B the result type of processing individual arguments through the view.
  * @tparam R the overall result type of aggregating all arguments, such as Seq[B]
  */
trait RepeatedView[-V[_],B,+R] {
  /** Function to process individual arguments. */
  protected def arg[A : V](a : A) : B
  /** Function to produce final results. */
  protected def result(b : B*) : R

  def apply() : R = result()
  def apply[A1 : V](a1 : A1) : R = result(arg(a1))
  def apply[A1 : V, A2 : V](a1 : A1, a2 : A2) : R = result(arg(a1), arg(a2))
  def apply[A1 : V, A2 : V, A3 : V](a1 : A1, a2 : A2, a3 : A3) : R = result(arg(a1), arg(a2), arg(a3))
  def apply[A1 : V, A2 : V, A3 : V, A4 : V](a1 : A1, a2 : A2, a3 : A3, a4 : A4) : R = result(arg(a1), arg(a2), arg(a3), arg(a4))
  def apply[A1 : V, A2 : V, A3 : V, A4 : V, A5 : V](a1 : A1, a2 : A2, a3 : A3, a4 : A4, a5 : A5) : R = result(arg(a1), arg(a2), arg(a3), arg(a4), arg(a5))
  def apply[A1 : V, A2 : V, A3 : V, A4 : V, A5 : V, A6 : V](a1 : A1, a2 : A2, a3 : A3, a4 : A4, a5 : A5, a6 : A6) : R = result(arg(a1), arg(a2), arg(a3), arg(a4), arg(a5), arg(a6))
  def apply[A1 : V, A2 : V, A3 : V, A4 : V, A5 : V, A6 : V, A7 : V](a1 : A1, a2 : A2, a3 : A3, a4 : A4, a5 : A5, a6 : A6, a7 : A7) : R = result(arg(a1), arg(a2), arg(a3), arg(a4), arg(a5), arg(a6), arg(a7))
  def apply[A1 : V, A2 : V, A3 : V, A4 : V, A5 : V, A6 : V, A7 : V, A8 : V](a1 : A1, a2 : A2, a3 : A3, a4 : A4, a5 : A5, a6 : A6, a7 : A7, a8 : A8) : R = result(arg(a1), arg(a2), arg(a3), arg(a4), arg(a5), arg(a6), arg(a7), arg(a8))

  def fromOption[A1 : V](a1 : Option[A1]) : R = a1.fold(apply())(apply(_))
}
