package dbrary.SQL

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db
import macros._

/** A wrapper for a table name useful for providing an implicit table context. */
case class FromTable(table : String) extends SimpleStatement(table)

/** A single expression for a select statement, where toString is expected to produce valid SQL. */
sealed class SelectExpr[A](expr : Statement)(implicit val sqlType : Type[A])
  extends StatementProxy(expr) {
  def get(a : Any) : A = Type.get[A](a)
  /** Qualify this expression to select from a particular table, if applicable. */
  def fromTable(implicit table : FromTable) : SelectExpr[A] = this
  def as(name : String) = new SelectAs(this, name)
}
object SelectExpr {
  def apply[A : Type](expr : String) = new SelectExpr[A](Statement(expr))
}

/** A simple "table.col" select expression. */
final case class SelectColumn[A : Type](table : String, col : String)
  extends SelectExpr[A](Statement(table + "." + col)) {
  override def fromTable(implicit table : FromTable) = copy(table = table.table)
  def column : Columns1[A] =
    new Columns1[A](this)(FromTable(table))
}
object SelectColumn {
  def apply[A : Type](col : String)(implicit table : FromTable) : SelectColumn[A] = SelectColumn[A](table.table, col)
}

/** A "expression AS name" select expression. */
final case class SelectAs[A : Type](expr : Statement, name : String)
  extends SelectExpr[A](expr + " AS " + name) {
  override def fromTable(implicit table : FromTable) = new SelectColumn[A](table.table, name)
}

/** Information necessary to run a simple SELECT query.
  * @param selects expressions to select in the query
  * @param source table name or other FROM expression
  * @param res parser for rows returned by the query
  */
case class Selector[+A](selects : Seq[SelectExpr[_]], source : Statement, joined : Statement, parse : Line[A]) {
  val length : Int = parse.arity.ensuring(_ == selects.length)
  def select = Statement.join(",", selects : _*)
  def statement : Statement =
    ("SELECT " +: select) ++ (" FROM " +: source)

  def ~[C : Type](a : SelectExpr[C]) : Selector[(A,C)] =
    copy[(A,C)](selects = selects :+ a, parse = parse.~[C](Cols[C]))
  def map[B](f : A => B) : Selector[B] =
    copy[B](parse = parse.map[B](f))
  private def ? : Selector[Option[A]] =
    copy[Option[A]](parse = parse.?)

  def from(src : Statement) : Selector[A] =
    copy[A](source = src, joined = "," +: src)
  def mapFrom(f : Statement => Statement) : Selector[A] =
    from(f(source))
  def fromQuery(query : Statement) : Selector[A] =
    from(("(" +: query) + ") AS " ++ source)
  def fromTable(implicit table : FromTable) : Selector[A] =
    copy[A](selects.map(_.fromTable(table))).from(table)
  def fromAlias(table : String) : Selector[A] =
    copy[A](selects.map(_.fromTable(FromTable(table)))).from(source + " AS " + table)

  def join[B](b : Selector[B]) : Selector[(A,B)] =
    new Selector[(A,B)](selects ++ b.selects, Statement.join("", source, b.joined), Statement.join("", joined, b.joined), parse.~[B](b.parse))
  def join[B,C](b : Selector[B], c : Selector[C]) : Selector[(A,B,C)] =
    new Selector[(A,B,C)](selects ++ b.selects ++ c.selects, Statement.join("", source, b.joined, c.joined), Statement.join("", joined, b.joined, c.joined), parse.~[B,C](b.parse, c.parse))
  def join[B,C,D](b : Selector[B], c : Selector[C], d : Selector[D]) : Selector[(A,B,C,D)] =
    new Selector[(A,B,C,D)](selects ++ b.selects ++ c.selects ++ d.selects, Statement.join("", source, b.joined, c.joined, d.joined), Statement.join("", joined, b.joined, c.joined, d.joined), parse.~[B,C,D](b.parse, c.parse, d.parse))
  def join[B,C,D,E](b : Selector[B], c : Selector[C], d : Selector[D], e : Selector[E]) : Selector[(A,B,C,D,E)] =
    new Selector[(A,B,C,D,E)](selects ++ b.selects ++ c.selects ++ d.selects ++ e.selects, Statement.join("", source, b.joined, c.joined, d.joined, e.joined), Statement.join("", joined, b.joined, c.joined, d.joined, e.joined), parse.~[B,C,D,E](b.parse, c.parse, d.parse, e.parse))

  protected def joiner(j : String, a : Statement = EmptyStatement) : Selector[A] =
    copy(joined = (j +: source) ++ a)
  def on(on : Statement) : Selector[A] =
    joiner(" JOIN ", " ON " +: on)
  def using(use : String*) : Selector[A] =
    joiner(" JOIN ", use.mkString(" USING (", ",", ")"))
  def using(use : Symbol) : Selector[A] =
    using(use.name)
  def on_?(on : Statement) : Selector[Option[A]] =
    ?.joiner(" LEFT JOIN ", " ON " +: on)
  def using_?(use : String*) : Selector[Option[A]] =
    ?.joiner(" LEFT JOIN ", use.mkString(" USING (", ",", ")"))
  def using_?(use : Symbol) : Selector[Option[A]] =
    using_?(use.name)
  def natural : Selector[A] =
    joiner(" NATURAL JOIN ")
  def natural_? : Selector[Option[A]] =
    ?.joiner(" NATURAL LEFT JOIN ")
  def cross : Selector[A] =
    joiner(" CROSS JOIN ")

  private def run(q : Query)(implicit dbc : db.Connection, executionContext : ExecutionContext) : Rows[A] =
    q.run[A](parse)
  def run(q : (Statement, Statement) => Query)(implicit dbc : db.Connection, executionContext : ExecutionContext) : Rows[A] =
    run(q(select, source))
  def SELECT(q : Query)(implicit dbc : db.Connection, executionContext : ExecutionContext) : Rows[A] =
    run((statement + " ") ++: q)
}

object Selector {
  def apply(selects : Seq[SelectExpr[_]])(implicit table : FromTable) : Selector[Seq[Any]] =
    new Selector[Seq[Any]](selects, table, "," +: table,
      new Line[Seq[Any]](selects.length, _.zip(selects).map { case (v, s) => s.get(v) }))
}

class SelectorProxy[A](val self : Selector[A])
  extends Selector[A](self.selects, self.source, self.joined, self.parse)
  with Proxy

abstract sealed class Columns[A,C <: Cols[A]] protected (selects : Seq[SelectExpr[_]], table : FromTable, override val parse : C)
  extends Selector[A](selects, table, "," +: table, parse) {
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
  extends Columns[Unit,Cols0](Nil, from, new Cols0) {
  override val length = 0
  def map[A](f : => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = new Columns1[C](a)
}
final class Columns1[C1](
    a1 : SelectExpr[C1]
  )(implicit from : FromTable)
  extends Columns[C1,Cols1[C1]](Seq(a1), from, new Cols1[C1]()(a1.sqlType)) {
  override val length = 1
  def ~+[C](a : SelectExpr[C]) = new Columns2[C1,C](a1,a)
}
final class Columns2[C1, C2](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  )(implicit from : FromTable)
  extends Columns[(C1,C2),Cols2[C1,C2]](Seq(a1, a2), from, new Cols2[C1,C2]()(a1.sqlType, a2.sqlType)) {
  override val length = 2
  def map[A](f : (C1,C2) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = new Columns3[C1,C2,C](a1,a2,a)
}
final class Columns3[C1, C2, C3](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3),Cols3[C1,C2,C3]](Seq(a1, a2, a3), from, new Cols3[C1,C2,C3]()(a1.sqlType, a2.sqlType, a3.sqlType)) {
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
  extends Columns[(C1,C2,C3,C4),Cols4[C1,C2,C3,C4]](Seq(a1, a2, a3, a4), from, new Cols4[C1,C2,C3,C4]()(a1.sqlType, a2.sqlType, a3.sqlType, a4.sqlType)) {
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
  extends Columns[(C1,C2,C3,C4,C5),Cols5[C1,C2,C3,C4,C5]](Seq(a1, a2, a3, a4, a5), from, new Cols5[C1,C2,C3,C4,C5]()(a1.sqlType, a2.sqlType, a3.sqlType, a4.sqlType, a5.sqlType)) {
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
  extends Columns[(C1,C2,C3,C4,C5,C6),Cols6[C1,C2,C3,C4,C5,C6]](Seq(a1, a2, a3, a4, a5, a6), from, new Cols6[C1,C2,C3,C4,C5,C6]()(a1.sqlType, a2.sqlType, a3.sqlType, a4.sqlType, a5.sqlType, a6.sqlType)) {
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
  extends Columns[(C1,C2,C3,C4,C5,C6,C7),Cols7[C1,C2,C3,C4,C5,C6,C7]](Seq(a1, a2, a3, a4, a5, a6, a7), from, new Cols7[C1,C2,C3,C4,C5,C6,C7]()(a1.sqlType, a2.sqlType, a3.sqlType, a4.sqlType, a5.sqlType, a6.sqlType, a7.sqlType)) {
  override val length = 7
  def map[A](f : (C1,C2,C3,C4,C5,C6,C7) => A) : Selector[A] = copy[A](parse = parse.map(f))
  def ~+[C](a : SelectExpr[C]) = ???
}
