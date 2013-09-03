package dbrary

import java.sql._
import anorm._

/*
abstract class ToTyStatement[T,A](implicit ts : ToStatement[T]) extends ToStatement[A] {
  def convert(a : A) : T
  def set(s : java.sql.PreparedStatement, index : Int, aValue : A) : Unit =
    ts.set(s, index, convert(aValue))
}
object ToTyStatement {
  implicit def identityTyStatement[A : ToStatement] : ToTyStatement[A,A] = new ToTyStatement[A,A] {
    def convert(a : A) = a
  }
  implicit def optionTyStatement[A : ToStatement] : ToTyStatement[Option[A],A] = new ToTyStatement[Option[A],A] {
    def convert(a : A) = Some(a)
  }
}
*/

class TySql(statement : PreparedStatement) extends Sql {
  def getFilledStatement(connection: java.sql.Connection, getGeneratedKeys: Boolean = false) =
    statement
}

abstract class TySqlQuery(val query : String)(implicit connection : Connection) {
  protected val statement = connection.prepareStatement(query)
}

object tynorm {
  import scala.language.experimental.macros
  import scala.reflect.macros.Context
  import macro._

  private class ArgumentMap[A](list : List[(String,A)]) {
    def toList = list
    def add(k : String, v : A, f : (A,A) => A) = {
      def listAdd(l : List[(String,A)]) : List[(String,A)] = l match {
        case Nil => List(k -> v)
        case ((x, a) :: l) if x == k => (x, f(a,v)) :: l
        case x :: l => x :: listAdd(l)
      }
      new ArgumentMap[A](listAdd(list))
    }
  }
  private object ArgumentMap {
    def apply[A](x : (String, A)*) : ArgumentMap[A] =
      new ArgumentMap[A](x.toList)
  }

  def TYSQL(queryString : String) = macro TYSQLImpl

  def TYSQLImpl(c : Context)(queryString : c.Expr[String]) : c.Expr[TySqlQuery] = {
    import c.universe._
    val (query, paramNames) = SqlStatementParser.parse(getString(c)(queryString))
    val stmt = Connection.prepareStatement(query)
    val pmeta = stmt.getParameterMetaData
    val rmeta = stmt.getMetaData
    stmt.close

    type TT = Tree
    def makeType(s : String, o : Boolean) : TT = {
      val t = Ident(c.mirror.staticClass(s))
      if (o) AppliedTypeTree(Ident("scala.Option"), List(t)) else t
    }
    def unifyTypes(p : String)(t1 : TT, t2 : TT) =
      if (t1.equalsStructure(t2))
        t1
      else
        c.abort(c.enclosingPosition, "tysql query parameter '" + p + "' is used twice with different types.")
    val ptypes = (1 to pmeta.getParameterCount).map { i =>
      makeType(pmeta.getParameterClassName(i),
        pmeta.isNullable(i) == ParameterMetaData.parameterNullable)
    }
    val param = paramNames.zip(ptypes).foldLeft(ArgumentMap[Tree]())((m, pt) =>
      m.add(pt._1, pt._2, unifyTypes(pt._1))).toList
    val rtypes = (1 to rmeta.getColumnCount).map { i =>
      makeType(rmeta.getColumnClassName(i),
        rmeta.isNullable(i) != ResultSetMetaData.columnNoNulls)
    }

    val ctn = newTypeName("$TySqlQuery")
    val statement = Select(This(c.mirror.staticClass("dbrary.TySqlQuery")), "statement")

    /* This doesn't work usefully because apply isn't exposed to the resulting type. */

    c.Expr(Block(
      List(ClassDef(Modifiers(Flag.FINAL), ctn, Nil, 
        Template(
          List(Ident(c.mirror.staticClass("dbrary.TySqlQuery"))), 
          emptyValDef, 
          List(
            DefDef(Modifiers(), nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(),
              Block(
                List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), 
                  List(Literal(Constant(query))))),
                Literal(Constant(())))),
            DefDef(Modifiers(), newTermName("apply"), List(), List(
                param.map({ case (p, t) =>
                  ValDef(Modifiers(Flag.PARAM), newTermName(p), t, EmptyTree)
                }),
                param.map({ case (p, t) =>
                  ValDef(Modifiers(Flag.PARAM | Flag.IMPLICIT), newTermName("_ts_" + p), 
                    AppliedTypeTree(Ident(c.mirror.staticClass("anorm.ToStatement")), List(t)), 
                    EmptyTree)
                })
              ),
              TypeTree(), 
              Block(
                paramNames.zipWithIndex.map({ case (p,i) =>
                  Apply(Select(Ident(newTermName("_ts_" + p)), "set"), List(statement, Literal(Constant(i+1)), Ident(newTermName(p))))
                }),
                Apply(Select(New(Ident(c.mirror.staticClass("dbrary.TySql"))), nme.CONSTRUCTOR),
                  List(statement))
              )
            )
          )
        )
      )),
      Apply(Select(New(Ident(ctn)), nme.CONSTRUCTOR), Nil)
    ))
  }
}
