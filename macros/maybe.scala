package macros

import scala.collection.GenTraversableOnce

/** Distinguish some ("defined"/"true") values from others ("empty"/"false"). */
trait Maybe[A] {
  def pass(a : A) : Boolean
  final def partial[B](f : A => B) : PartialFunction[A,B] = new PartialFunction[A,B] {
    def isDefinedAt(a : A) = pass(a)
    def apply(a : A) = f(a)
  }
  final def opt(a : A) : Option[A] =
    if (pass(a)) Some(a) else None
  final def map[B >: A](f : A => B, a : A) : B =
    if (pass(a)) f(a) else a
}

/** Utility that creates [[scala.Option]]s out of values. */
object Maybe {
  def apply[A](p : A => Boolean) = new Maybe[A] {
    def pass(a : A) = p(a)
  }

  implicit val trueBoolean : Maybe[Boolean] = Maybe[Boolean](a => a)
  implicit val someOption : Maybe[Option[_]] = Maybe[Option[_]](_.isDefined)
  implicit val nonEmptyTraversable : Maybe[GenTraversableOnce[_]] = Maybe[GenTraversableOnce[_]](_.nonEmpty)
  implicit val nonEmptyString : Maybe[String] = Maybe[String](_.nonEmpty)
  /* useful with String.index */
  implicit val nonNegativeInt : Maybe[Int] = Maybe[Int](_ >= 0)

  def pass[A](a : A)(implicit m : Maybe[A]) : Boolean = m.pass(a)

  def partial[A,B](f : A => B)(implicit m : Maybe[A]) : PartialFunction[A,B] = m.partial(f)

  /** A more concise version of the common `Some(_).filter(_)` idiom.
    * @return Some(a) if f(a), None otherwise
    */
  def opt[A](a : A)(implicit m : Maybe[A]) : Option[A] = m.opt(a)

  /** Conditionally map only true values. */
  def map[A](f : A => A, a : A)(implicit m : Maybe[A]) : A = m.map(f, a)

  def bracket(l : String = "", a : String, r : String = "")(implicit m : Maybe[String]) : String =
    m.map(l + _ + r, a)

  /** Compute the value, usually a string parse, catching NumberFormatException.
    * @return Some(f) unless f throws NumberFormatException
    */
  def toNumber[A](f : => A) : Option[A] =
    scala.util.control.Exception.catching(classOf[java.lang.NumberFormatException]).opt(f)
  def toInt(s : String) : Option[Int] = toNumber(s.toInt)
  def toLong(s : String) : Option[Long] = toNumber(s.toLong)
  def toDouble(s : String) : Option[Double] = toNumber(s.toDouble)
}
