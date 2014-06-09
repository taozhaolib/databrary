package macros

object Cast {
  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  def castImpl[A : c.WeakTypeTag](c : Context)(x : c.Expr[Any]) = {
    import c.universe._
    val t = weakTypeOf[A]
    q"$x match { case a : $t => Some(a) ; case _ => None }"
  }
}

