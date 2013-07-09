package dbrary

import scala.reflect.macros.Context

private [dbrary] object macro {
  def getString(c : Context)(str : c.Expr[String]) : String = str.tree match {
    case c.universe.Literal(c.universe.Constant(s : String)) => s
    case _ => c.abort(c.enclosingPosition, "Argument must be a string literal")
  }
}
