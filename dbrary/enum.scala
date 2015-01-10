package dbrary.SQL

import scala.util.control.Exception.catching
import play.api.libs.json
import macros._

/** Enumerations which reflect types in the database.
  * Any Enum should exactly match the correspending database type. */
abstract class Enum(name : String) extends Enumeration {
  implicit val sqlType : Type[Value] =
    Type[Value](name, classOf[Value])(withNameOpt, _.toString)
  implicit val jsonFormat : json.Format[Value] = new json.Format[Value] {
    def writes(v : Value) = json.JsNumber(v.id)
    def reads(j : json.JsValue) = j match {
      case json.JsNumber(i) if i.isValidInt && i >= 0 && i < maxId => json.JsSuccess(apply(i.toInt))
      case json.JsString(s) => fromString(s).fold[json.JsResult[Value]](json.JsError("error.expected.jsnumber"))(json.JsSuccess(_))
      case _ => json.JsError("error.expected.jsnumber")
    }
  }
  final def withNameOpt(name : String) : Option[Value] =
    catching(classOf[NoSuchElementException]).opt(withName(name))
  final def fromString(s : String) : Option[Value] =
    withNameOpt(s) orElse
      Maybe.toInt(s).flatMap(i => catching(classOf[NoSuchElementException]).opt(apply(i)))
}

object Enum {
  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context
  import Macro._

  /* This is not very useful as it can only create structural values rather than top-level objects */
  def make(enumName : String) : Enum = macro makeImpl

  def makeImpl(c : Context)(enumName : c.Expr[String]) : c.Expr[Enum] = {
    import c.universe._
    val name = getString(c)(enumName)
    val labels = Connection.Static.enumLabels(name)
    val obj = TermName(name)

    c.Expr(Block(
      List(ModuleDef(Modifiers(), obj, Template(
        List(Ident(c.mirror.staticClass("dbrary.SQL.Enum"))),
        noSelfType,
        DefDef(Modifiers(), termNames.CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(
          List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), 
            List(enumName.tree))),
          Literal(Constant(()))))
        :: labels.map(l => 
          ValDef(Modifiers(), TermName(l), TypeTree(), Select(This(TypeName("")), TermName("Value"))))
      ))),
      Ident(obj)
    ))
  }
}
