package macros

object Cast {
  import scala.language.experimental.macros
  import scala.reflect.macros.Context

  def castImpl[A : c.WeakTypeTag](c : Context)(x : c.Expr[Any]) : c.Expr[Option[A]] = {
    import c.universe._
    val t = implicitly[WeakTypeTag[A]]
    val a = c.Expr[A](Ident(newTermName("a")))
    val sa = reify(Some(a.splice))
    val n = reify(None)
    val i = Ident(nme.WILDCARD)
    c.Expr[Option[A]] {
      Match(x.tree, List(
        CaseDef(Bind(newTermName("a"), Typed(i, TypeTree(t.tpe))), EmptyTree, sa.tree),
        CaseDef(i, EmptyTree, n.tree)
      ))
    }
  }
}

