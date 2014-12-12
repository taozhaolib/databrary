package dbrary

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db
import macros._

/** A wrapper for a table name useful for providing an implicit table context. */
case class FromTable(table : String) {
  override def toString : String = table
}
object FromTable {
  import scala.language.implicitConversions
  implicit def table(table : FromTable) : String = table.table
}

/** A single expression for a select statement, where toString is expected to produce valid SQL. */
sealed class SelectExpr[A](expr : String)(implicit val sqlType : SQLType[A]) {
  final override def toString : String = expr
  def get(a : Any) : A = SQLType.get[A](a)
  /** Qualify this expression to select from a particular table, if applicable. */
  def fromTable(implicit table : FromTable) : SelectExpr[A] = this
}
object SelectExpr {
  def apply[A : SQLType](expr : String) = new SelectExpr[A](expr)
}

/** A simple "table.col" select expression. */
final case class SelectColumn[A : SQLType](table : String, col : String) extends SelectExpr[A](table + "." + col) {
  override def fromTable(implicit table : FromTable) = copy(table = table)
  def column : Columns1[A] =
    new Columns1[A](this)(FromTable(table))
}
object SelectColumn {
  def apply[A : SQLType](col : String)(implicit table : FromTable) : SelectColumn[A] = SelectColumn[A](table, col)
}

/** A "expression AS name" select expression. */
final case class SelectAs[A : SQLType](expr : String, name : String) extends SelectExpr[A](expr + " AS " + name) {
  override def fromTable(implicit table : FromTable) = SelectColumn[A](name)
}

/** Information necessary to run a simple SELECT query.
  * @param selects expressions to select in the query
  * @param source table name or other FROM expression
  * @param res parser for rows returned by the query
  */
case class Selector[+A](selects : Seq[SelectExpr[_]], source : String, parse : SQLLine[A], preargs : SQLArgs = SQLNoArgs) {
  def select = selects.mkString(", ")
  val length : Int = parse.arity.ensuring(_ == selects.length)

  def ~[C : SQLType](a : SelectExpr[C]) : Selector[(A,C)] =
    copy[(A,C)](selects = selects :+ a, parse = parse.~[C](SQLCols[C]))
  def map[B](f : A => B) : Selector[B] =
    copy[B](parse = parse.map[B](f))
  protected def ? : Selector[Option[A]] =
    copy[Option[A]](parse = parse.?)
  def from(from : String) : Selector[A] =
    copy[A](source = from)
  def from(from : String => String) : Selector[A] =
    copy[A](source = from(source))
  def from(query : SQLToRows[_]) : Selector[A] =
    copy[A](source = "(" + query.query + ") AS " + source, preargs = preargs ++ query.preargs)
  def fromTable(implicit table : FromTable) : Selector[A] =
    copy[A](selects.map(_.fromTable(table)), source = table)
  def fromAlias(table : String) : Selector[A] =
    copy[A](selects.map(_.fromTable(FromTable(table))), source = source + " AS " + table)
  /** Add arguments needed for the select expressions that will be passed (first) to any queries executed. */
  def pushArgs(args : SQLArgs) : Selector[A] =
    copy[A](preargs = preargs ++ args)

  def join[B](b : Joiner[B]) : Selector[(A,B)] =
    new Selector[(A,B)](selects ++ b.selects, source + " " + b.join, parse.~[B](b.parse), preargs ++ b.preargs)
  def join[B,C](b : Joiner[B], c : Joiner[C]) : Selector[(A,B,C)] =
    new Selector[(A,B,C)](selects ++ b.selects ++ c.selects, unwords(source, b.join, c.join), parse.~[B,C](b.parse, c.parse), preargs ++ b.preargs ++ c.preargs)
  def *[B](that : Selector[B]) : Selector[(A,B)] =
    join(that.cross)

  private[dbrary] def joiner(join : String*) : Joiner[A] =
    new Joiner[A](selects, source, unwords(join : _*), parse, preargs)
  def on(on : String) : Joiner[A] =
    joiner("JOIN", source, "ON", on)
  def using(use : String*) : Joiner[A] =
    joiner("JOIN", source, "USING", use.mkString("(", ",", ")"))
  def using(use : Symbol) : Joiner[A] =
    using(use.name)
  def on_?(on : String) : Joiner[Option[A]] =
    ?.joiner("LEFT JOIN", source, "ON", on)
  def using_?(use : String*) : Joiner[Option[A]] =
    ?.joiner("LEFT JOIN", source, "USING", use.mkString("(", ",", ")"))
  def using_?(use : Symbol) : Joiner[Option[A]] =
    using_?(use.name)
  def natural : Joiner[A] =
    joiner("NATURAL JOIN", source)
  def natural_? : Joiner[Option[A]] =
    ?.joiner("NATURAL LEFT JOIN", source)
  def cross : Joiner[A] =
    joiner("CROSS JOIN", source)

  def SQL(q : (String, String) => String)(implicit dbc : db.Connection, executionContext : ExecutionContext) : SQLToRows[A] =
    new SQLToRows(q(select, source), parse, true, preargs.args)(dbc, executionContext)
  def SELECT(q : String*)(implicit dbc : db.Connection, executionContext : ExecutionContext) : SQLToRows[A] =
    SQL((select, source) => unwords(Seq("SELECT", select, "FROM", source) ++ q : _*))
}

object Selector {
  def apply(selects : Seq[SelectExpr[_]])(implicit table : FromTable) : Selector[Seq[Any]] =
    new Selector[Seq[Any]](selects, table,
      new SQLLine[Seq[Any]](selects.length, _.zip(selects).map { case (v, s) => s.get(v) }))
}

final class Joiner[+A](selects : Seq[SelectExpr[_]], source : String, val join : String, parse : SQLLine[A], preargs : SQLArgs = SQLNoArgs)
  extends Selector[A](selects, source, parse, preargs) {
  override def join[B](b : Joiner[B]) : Joiner[(A,B)] =
    super.join(b).joiner(join + " " + b.join)
  override def join[B,C](b : Joiner[B], c : Joiner[C]) : Joiner[(A,B,C)] =
    super.join(b, c).joiner(unwords(join, b.join, c.join))
}

abstract sealed class Columns[A,C <: SQLCols[A]] protected (selects : Seq[SelectExpr[_]], table : FromTable, override val parse : C) extends Selector[A](selects, table, parse) {
  def ~+[C2](a : SelectExpr[C2]) : Columns[_,_]
}

object Columns {
  def apply(implicit from : FromTable) = new Columns0
  def apply[C1](a1 : SelectExpr[C1])(implicit from : FromTable) =
    new Columns1[C1](a1)
  def apply[C1, C2](a1 : SelectExpr[C1], a2 : SelectExpr[C2])(implicit from : FromTable) =
    new Columns2[C1,C2](a1,a2)
  def apply[C1, C2, C3](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3])(implicit from : FromTable) =
    new Columns3[C1,C2,C3](a1,a2,a3)
  def apply[C1, C2, C3, C4](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3], a4 : SelectExpr[C4])(implicit from : FromTable) =
    new Columns4[C1,C2,C3,C4](a1,a2,a3,a4)
  def apply[C1, C2, C3, C4, C5](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3], a4 : SelectExpr[C4], a5 : SelectExpr[C5])(implicit from : FromTable) =
    new Columns5[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)
  def apply[C1, C2, C3, C4, C5, C6](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3], a4 : SelectExpr[C4], a5 : SelectExpr[C5], a6 : SelectExpr[C6])(implicit from : FromTable) =
    new Columns6[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)
  def apply[C1, C2, C3, C4, C5, C6, C7](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3], a4 : SelectExpr[C4], a5 : SelectExpr[C5], a6 : SelectExpr[C6], a7 : SelectExpr[C7])(implicit from : FromTable) =
    new Columns7[C1,C2,C3,C4,C5,C6,C7](a1,a2,a3,a4,a5,a6,a7)
}

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
final class Columns0(implicit from : FromTable)
  extends Columns[Unit,SQLCols0](Nil, from, new SQLCols0) {
  override val length = 0
  def map[A](f : => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = new Columns1[C](a)
}
final class Columns1[C1](
    a1 : SelectExpr[C1]
  )(implicit from : FromTable)
  extends Columns[C1,SQLCols1[C1]](Seq(a1), from, new SQLCols1[C1]()(a1.sqlType)) {
  override val length = 1
  def ~+[C](a : SelectExpr[C]) = new Columns2[C1,C](a1,a)
}
final class Columns2[C1, C2](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  )(implicit from : FromTable)
  extends Columns[(C1,C2),SQLCols2[C1,C2]](Seq(a1, a2), from, new SQLCols2[C1,C2]()(a1.sqlType, a2.sqlType)) {
  override val length = 2
  def map[A](f : (C1,C2) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = new Columns3[C1,C2,C](a1,a2,a)
}
final class Columns3[C1, C2, C3](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3),SQLCols3[C1,C2,C3]](Seq(a1, a2, a3), from, new SQLCols3[C1,C2,C3]()(a1.sqlType, a2.sqlType, a3.sqlType)) {
  override val length = 3
  def map[A](f : (C1,C2,C3) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = new Columns4[C1,C2,C3,C](a1,a2,a3,a)
}
final class Columns4[C1, C2, C3, C4](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  , a4 : SelectExpr[C4]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4),SQLCols4[C1,C2,C3,C4]](Seq(a1, a2, a3, a4), from, new SQLCols4[C1,C2,C3,C4]()(a1.sqlType, a2.sqlType, a3.sqlType, a4.sqlType)) {
  override val length = 4
  def map[A](f : (C1,C2,C3,C4) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = new Columns5[C1,C2,C3,C4,C](a1,a2,a3,a4,a)
}
final class Columns5[C1, C2, C3, C4, C5](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  , a4 : SelectExpr[C4]
  , a5 : SelectExpr[C5]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5),SQLCols5[C1,C2,C3,C4,C5]](Seq(a1, a2, a3, a4, a5), from, new SQLCols5[C1,C2,C3,C4,C5]()(a1.sqlType, a2.sqlType, a3.sqlType, a4.sqlType, a5.sqlType)) {
  override val length = 5
  def map[A](f : (C1,C2,C3,C4,C5) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = new Columns6[C1,C2,C3,C4,C5,C](a1,a2,a3,a4,a5,a)
}
final class Columns6[C1, C2, C3, C4, C5, C6](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  , a4 : SelectExpr[C4]
  , a5 : SelectExpr[C5]
  , a6 : SelectExpr[C6]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5,C6),SQLCols6[C1,C2,C3,C4,C5,C6]](Seq(a1, a2, a3, a4, a5, a6), from, new SQLCols6[C1,C2,C3,C4,C5,C6]()(a1.sqlType, a2.sqlType, a3.sqlType, a4.sqlType, a5.sqlType, a6.sqlType)) {
  override val length = 6
  def map[A](f : (C1,C2,C3,C4,C5,C6) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = new Columns7[C1,C2,C3,C4,C5,C6,C](a1,a2,a3,a4,a5,a6,a)
}
final class Columns7[C1, C2, C3, C4, C5, C6, C7](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  , a4 : SelectExpr[C4]
  , a5 : SelectExpr[C5]
  , a6 : SelectExpr[C6]
  , a7 : SelectExpr[C7]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5,C6,C7),SQLCols7[C1,C2,C3,C4,C5,C6,C7]](Seq(a1, a2, a3, a4, a5, a6, a7), from, new SQLCols7[C1,C2,C3,C4,C5,C6,C7]()(a1.sqlType, a2.sqlType, a3.sqlType, a4.sqlType, a5.sqlType, a6.sqlType, a7.sqlType)) {
  override val length = 7
  def map[A](f : (C1,C2,C3,C4,C5,C6,C7) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = ???
}
