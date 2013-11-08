package object macros {
  import scala.language.experimental.macros
  /** Cast Any to Option[A].  Equivalent to: `cast[A](x) = x match { a : A => Some(a) ; _ => None }` */
  def cast[A](x : Any) = macro Cast.castImpl[A]

  /** What a.zip(b) should do but doesn't? */
  def optionZip[A,B](a : Option[A], b : Option[B]) : Option[(A,B)] = (a, b) match {
    case (Some(a), Some(b)) => Some(a, b)
    case _ => None
  }

  def unwords(s : String*) = s.mkString(" ")
}
