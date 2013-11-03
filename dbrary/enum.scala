package dbrary

import scala.util.control.Exception.catching
import macros._

/** Enumerations which reflect types in the database.
  * Any PGEnum should exactly match the correspending database type. */
abstract class PGEnum(name : String) extends Enumeration {
  val sqlType : SQLType[Value] =
    SQLType[Value](name, classOf[Value]) { s =>
      catching(classOf[NoSuchElementException]).opt(withName(s))
    } {
      _.toString
    }
}

object PGEnum {
  import scala.language.experimental.macros
  import scala.reflect.macros.Context
  import macro._

  /* This is not very useful as it can only create structural values rather than top-level objects */
  def make(enumName : String) = macro makeImpl

  def makeImpl(c : Context)(enumName : c.Expr[String]) : c.Expr[Any] = {
    import c.universe._
    val name = getString(c)(enumName)
    val labels = Connection.Static.enumLabels(name)
    val obj = newTermName(name)

    c.Expr(Block(
      List(ModuleDef(Modifiers(), obj, Template(
        List(Ident(c.mirror.staticClass("dbrary.PGEnum"))),
        emptyValDef,
        DefDef(Modifiers(), nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(
          List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), 
            List(enumName.tree))),
          Literal(Constant(()))))
        :: labels.map(l => 
          ValDef(Modifiers(), newTermName(l), TypeTree(), Select(This(""), newTermName("Value"))))
      ))),
      Ident(obj)
    ))
  }
}
