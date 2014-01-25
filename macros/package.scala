package object macros {
  import scala.language.experimental.macros
  /** Cast Any to Option[A].  Equivalent to: `cast[A](x) = x match { a : A => Some(a) ; _ => None }` */
  def cast[A](x : Any) = macro Cast.castImpl[A]

  /** What a.zip(b) should do but doesn't?
    * Takes an optional function to map the result over. */
  def zip[A,B,C](a : Option[A], b : Option[B], f : (A, B) => C = Tuple2.apply _) : Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  /** Apply a function to both components of a homogenous tuple. */
  def both[A,B](a : (A, A), f : A => B) : (B, B) =
    (f(a._1), f(a._2))

  def unwords(s : String*) = s.mkString(" ")

  def const[A](x : A) : Any => A = _ => x

  def apply[A,B](f : A => B, a : A) : B = f(a)
  def tupleApply[A,B](fa : (A => B, A)) : B = fa._1(fa._2)
}
