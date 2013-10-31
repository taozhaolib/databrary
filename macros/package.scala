package object macros {
  import scala.language.experimental.macros
  /** Cast Any to Option[A].  Equivalent to: `cast[A](x) = x match { a : A => Some(a) ; _ => None }` */
  def cast[A](x : Any) = macro Cast.castImpl[A]

  def unwords(s : String*) = s.mkString(" ")
}
