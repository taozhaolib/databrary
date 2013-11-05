package dbrary

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db
import macros._

/** A wrapper for a table name useful for providing an implicit table context. */
case class FromTable(table : String) {
  implicit override def toString : String = table
}

/** A single expression for a select statement, where toString is expected to produce valid SQL. */
sealed class SelectExpr[A : SQLType](val expr : String) {
  final override def toString : String = expr
  def get(a : Any) : A = SQLType.get[A](a)
  /** Qualify this expression to select from a particular table, if applicable. */
  def fromTable(table : String) : SelectExpr[A] = this
}
object SelectExpr {
  def apply[A : SQLType](expr : String) = new SelectExpr[A](expr)
}

/** A simple "table.col" select expression. */
final case class SelectColumn[A : SQLType](table : String, col : String) extends SelectExpr[A](table + "." + col) {
  override def fromTable(table : String) = copy(table = table)
  private[this] implicit lazy val fromTable : FromTable = FromTable(table)
  implicit def column : Columns1[A] = new Columns1[A](this)
}
object SelectColumn {
  def apply[A : SQLType](col : String)(implicit from : FromTable) : SelectColumn[A] = SelectColumn[A](from.table, col)
}

/** A "expression AS name" select expression. */
final case class SelectAs[A : SQLType](override val expr : String, name : String) extends SelectExpr[A](expr + " AS " + name)

/** Information necessary to run a simple SELECT query.
  * @param selects expressions to select in the query
  * @param source table name or other FROM expression
  * @param res parser for rows returned by the query
  */
case class Selector[A](selects : Seq[SelectExpr[_]], source : String, parse : SQLLine[A]) {
  def select = selects.mkString(", ")
  val length : Int = parse.arity.ensuring(_ == selects.length)

  def map[B](f : A => B) : Selector[B] =
    copy[B](parse = parse.map[B](f))
  def ? : Selector[Option[A]] =
    copy[Option[A]](parse = parse.?)
  def from(from : String) : Selector[A] =
    copy[A](source = from)
  def fromAlias(name : String) : Selector[A] =
    copy[A](selects.map(_.fromTable(name)), source = source + " AS " + name)

  def join[B](that : Selector[B], joiner : (String, String) => String) : Selector[(A,B)] =
    Selector[(A,B)](selects ++ that.selects, joiner(source, that.source), parse.~[B](that.parse))
  def join[B](that : Selector[B], on : String) : Selector[(A,B)] =
    join(that, unwords(_, "JOIN", _, "ON", on))
  def join[B](that : Selector[B], using : Seq[String]) : Selector[(A,B)] =
    join(that, unwords(_, "JOIN", _, "USING", using.mkString("(", ", ", ")")))
  def join[B](that : Selector[B], using : Symbol) : Selector[(A,B)] =
    join(that, Seq(using.name))
  def leftJoin[B](that : Selector[B], on : String) : Selector[(A,Option[B])] =
    join(that.?, unwords(_, "LEFT JOIN", _, "ON", on))
  def leftJoin[B](that : Selector[B], using : Seq[String]) : Selector[(A,Option[B])] =
    join(that.?, unwords(_, "LEFT JOIN", _, "USING", using.mkString("(", ", ", ")")))
  def leftJoin[B](that : Selector[B], using : Symbol) : Selector[(A,Option[B])] =
    leftJoin(that, Seq(using.name))
  def ~[B](that : Selector[B]) : Selector[(A,B)] =
    join(that, _ + " NATURAL JOIN " + _)

  private[this] def selectStmt(q : Seq[String]) : String = unwords(Seq("SELECT", select, "FROM", source) ++ q : _*)
  def SELECT(q : String*)(implicit dbc : db.Connection, executionContext : ExecutionContext) : SQLToRows[A] =
    SQLToRows(selectStmt(q), parse)(dbc, executionContext)
}

object Selector {
  def apply(selects : Seq[SelectExpr[_]])(implicit from : FromTable) : Selector[Seq[Any]] =
    new Selector[Seq[Any]](selects, from.table,
      new SQLLine[Seq[Any]](selects.length, _.zip(selects).map { case (v, s) => s.get(v) }))
}

abstract sealed class Columns[A,C <: SQLCols[A]] protected (selects : Seq[SelectExpr[_]], from : FromTable, override val parse : C) extends Selector[A](selects, from.table, parse) {
  def ~+[C : SQLType](a : SelectExpr[C]) : Columns[_,_]
}

object Columns {
  def apply(implicit from : FromTable) = new Columns0
  def apply[C1 : SQLType](a1 : SelectExpr[C1])(implicit from : FromTable) =
    new Columns1[C1](a1)
  def apply[C1 : SQLType, C2 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2])(implicit from : FromTable) =
    new Columns2[C1,C2](a1,a2)
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3])(implicit from : FromTable) =
    new Columns3[C1,C2,C3](a1,a2,a3)
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3], a4 : SelectExpr[C4])(implicit from : FromTable) =
    new Columns4[C1,C2,C3,C4](a1,a2,a3,a4)
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3], a4 : SelectExpr[C4], a5 : SelectExpr[C5])(implicit from : FromTable) =
    new Columns5[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType, C6 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3], a4 : SelectExpr[C4], a5 : SelectExpr[C5], a6 : SelectExpr[C6])(implicit from : FromTable) =
    new Columns6[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType, C6 : SQLType, C7 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3], a4 : SelectExpr[C4], a5 : SelectExpr[C5], a6 : SelectExpr[C6], a7 : SelectExpr[C7])(implicit from : FromTable) =
    new Columns7[C1,C2,C3,C4,C5,C6,C7](a1,a2,a3,a4,a5,a6,a7)
}

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
final class Columns0(implicit from : FromTable)
  extends Columns[Unit,SQLCols0](Nil, from, new SQLCols0) {
  override val length = 0
  def map[A](f : => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns1[C](a)
}
final class Columns1[C1 : SQLType](
    a1 : SelectExpr[C1]
  )(implicit from : FromTable)
  extends Columns[C1,SQLCols1[C1]](Seq(a1), from, new SQLCols1[C1]) {
  override val length = 1
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns2[C1,C](a1,a)
}
final class Columns2[C1 : SQLType, C2 : SQLType](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  )(implicit from : FromTable)
  extends Columns[(C1,C2),SQLCols2[C1,C2]](Seq(a1, a2), from, new SQLCols2[C1,C2]) {
  override val length = 2
  def map[A](f : (C1,C2) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns3[C1,C2,C](a1,a2,a)
}
final class Columns3[C1 : SQLType, C2 : SQLType, C3 : SQLType](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3),SQLCols3[C1,C2,C3]](Seq(a1, a2, a3), from, new SQLCols3[C1,C2,C3]) {
  override val length = 3
  def map[A](f : (C1,C2,C3) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns4[C1,C2,C3,C](a1,a2,a3,a)
}
final class Columns4[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  , a4 : SelectExpr[C4]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4),SQLCols4[C1,C2,C3,C4]](Seq(a1, a2, a3, a4), from, new SQLCols4[C1,C2,C3,C4]) {
  override val length = 4
  def map[A](f : (C1,C2,C3,C4) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns5[C1,C2,C3,C4,C](a1,a2,a3,a4,a)
}
final class Columns5[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  , a4 : SelectExpr[C4]
  , a5 : SelectExpr[C5]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5),SQLCols5[C1,C2,C3,C4,C5]](Seq(a1, a2, a3, a4, a5), from, new SQLCols5[C1,C2,C3,C4,C5]) {
  override val length = 5
  def map[A](f : (C1,C2,C3,C4,C5) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns6[C1,C2,C3,C4,C5,C](a1,a2,a3,a4,a5,a)
}
final class Columns6[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType, C6 : SQLType](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  , a4 : SelectExpr[C4]
  , a5 : SelectExpr[C5]
  , a6 : SelectExpr[C6]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5,C6),SQLCols6[C1,C2,C3,C4,C5,C6]](Seq(a1, a2, a3, a4, a5, a6), from, new SQLCols6[C1,C2,C3,C4,C5,C6]) {
  override val length = 6
  def map[A](f : (C1,C2,C3,C4,C5,C6) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns7[C1,C2,C3,C4,C5,C6,C](a1,a2,a3,a4,a5,a6,a)
}
final class Columns7[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType, C6 : SQLType, C7 : SQLType](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  , a4 : SelectExpr[C4]
  , a5 : SelectExpr[C5]
  , a6 : SelectExpr[C6]
  , a7 : SelectExpr[C7]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5,C6,C7),SQLCols7[C1,C2,C3,C4,C5,C6,C7]](Seq(a1, a2, a3, a4, a5, a6, a7), from, new SQLCols7[C1,C2,C3,C4,C5,C6,C7]) {
  override val length = 7
  def map[A](f : (C1,C2,C3,C4,C5,C6,C7) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = ???
}
