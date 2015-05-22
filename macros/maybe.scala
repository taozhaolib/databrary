package macros

import scala.collection.GenTraversableOnce

/** Distinguish some ("defined"/"true") values from others ("empty"/"false"). */
trait Truth[A] {
  def pass(a : A) : Boolean
  final def partial[B](f : A => B) : PartialFunction[A,B] = new PartialFunction[A,B] {
    def isDefinedAt(a : A) = pass(a)
    def apply(a : A) = f(a)
  }
}

object Truth {
  def apply[A](p : A => Boolean) = new Truth[A] {
    def pass(a : A) = p(a)
  }

  import scala.language.higherKinds
  def all[A] : Truth[A] = Truth[A](_ => true)
  def none[A] : Truth[A] = Truth[A](_ => false)
  implicit val trueBoolean : Truth[Boolean] = Truth[Boolean](a => a)
  implicit def someOption[A] : Truth[Option[A]] = Truth[Option[A]](_.isDefined)
  implicit def nonEmptyTraversable[A, T[A] <: GenTraversableOnce[A]] : Truth[T[A]] = Truth[T[A]](_.nonEmpty)
  def existsTraversable[A, T[A] <: GenTraversableOnce[A]](implicit t : Truth[A]) : Truth[T[A]] = Truth[T[A]](_.exists(t.pass))
  def forallTraversable[A, T[A] <: GenTraversableOnce[A]](implicit t : Truth[A]) : Truth[T[A]] = Truth[T[A]](_.forall(t.pass))
  implicit val nonEmptyString : Truth[String] = Truth[String](_.nonEmpty)
  /* useful with String.index */
  implicit val nonNegativeInt : Truth[Int] = Truth[Int](_ >= 0)
}

/** A passively conditional value based on a Truth instance. */
final case class Maybe[+A](a : A)(implicit t : Truth[A]) {
  final def pass : Boolean = t.pass(a)
  /** A more concise version of the common `Some(_).filter(_)` idiom.
    * @return Some(a) if pass(a), None otherwise
    */
  final def opt[B](f : A => B = identity[A] _) : Option[B] =
    if (pass) Some(f(a)) else None
  /** Conditionally map only true values. */
  final def andThen[B >: A](f : A => B) : B =
    if (pass) f(a) else a
  final def orElse[B >: A](b : => B) : B =
    if (pass) a else b
  final def fold[B](b : => B)(f : A => B) : B =
    if (pass) f(a) else b
}

object Maybe {
  import scala.language.implicitConversions
  implicit def pass(m : Maybe[_]) : Boolean = m.pass
  implicit def opt[A](m : Maybe[A]) : Option[A] = m.opt()

  def bracket(l : String = "", a : String, r : String = "")(implicit t : Truth[String]) : String =
    Maybe(a)(t).andThen(l + _ + r)
  def join(l : String, j : String, r : String)(implicit t : Truth[String]) : String =
    if (t.pass(l) && t.pass(r)) l + j + r
    else l + r
  def flatOpt[A](o : Option[A])(implicit t : Truth[A]) : Option[A] =
    o.filter(t.pass)

  /** Compute the value, usually a string parse, catching NumberFormatException.
    * @return Some(f) unless f throws NumberFormatException
    */
  def toNumber[A](f : => A) : Option[A] =
    scala.util.control.Exception.catching(classOf[java.lang.NumberFormatException]).opt(f)
  def toInt(s : String) : Option[Int] = toNumber(s.toInt)
  def toLong(s : String) : Option[Long] = toNumber(s.toLong)
  def toDouble(s : String) : Option[Double] = toNumber(s.toDouble)
}
