package dbrary

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db
import macros._

case class FromTable(table : String) {
  implicit override def toString : String = table
}

sealed class SelectExpr[A](val expr : String) {
  final override def toString : String = expr
  def inTable(table : String) : SelectExpr[A] = this
}
object SelectExpr {
  def apply[A : SQLType](expr : String) = new SelectExpr[A](expr)
}

final case class SelectColumn[A](table : String, col : String) extends SelectExpr[A](table + "." + col) {
  override def inTable(table : String) = copy(table = table)
}
object SelectColumn {
  def apply[A : SQLType](col : String)(implicit from : FromTable) : SelectColumn[A] = SelectColumn[A](from.table, col)
}

final case class SelectAs[A](override val expr : String, name : String) extends SelectExpr[A](expr + " AS " + name)

case class Selector[A](selects : Seq[SelectExpr[_]], source : String, res : SQLResub[A]) {
  def select = selects.mkString(", ")

  def map[B](f : A => B) : Selector[B] =
    copy[B](res = res.map[B](f))
  def ? : Selector[Option[A]] =
    copy[Option[A]](res = res.?)
  def from(from : String) : Selector[A] =
    copy[A](source = from)

  def join[B](that : Selector[B], joiner : (String, String) => String) : Selector[(A,B)] =
    Selector[(A,B)](selects ++ that.selects, joiner(source, that.source), res.~[B](that.res))
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

  private[this] def selectStmt(q : String*) : String = unwords(Seq("SELECT", select, "FROM", source) ++ q : _*)
  def SELECT(q : String*)(args : SQLArgs)(implicit dbc : db.Connection, executionContext : ExecutionContext) : SQLFuture[A] =
    args.query(selectStmt(q : _*)).as(res)
}

abstract sealed class Columns[A,C <: SQLCols[A]] protected (selects : Seq[SelectExpr[_]], from : FromTable, override val res : C) extends Selector[A](selects, from.table, res) {
  def ~+[C : SQLType](a : SelectExpr[C]) : Columns[_,_]
}

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
final class Columns0(implicit from : FromTable)
  extends Columns[Unit,SQLCols0](Nil, from, new SQLCols0) {
  def map[A](f : => A) : Selector[A] = copy[A](res = res.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns1[C](a)
}
final class Columns1[C1 : SQLType](
    a1 : SelectExpr[C1]
  )(implicit from : FromTable)
  extends Columns[C1,SQLCols1[C1]](Seq(a1), from, new SQLCols1[C1]) {
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns2[C1,C](a1,a)
}
final class Columns2[C1 : SQLType, C2 : SQLType](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  )(implicit from : FromTable)
  extends Columns[(C1,C2),SQLCols2[C1,C2]](Seq(a1, a2), from, new SQLCols2[C1,C2]) {
  def map[A](f : (C1,C2) => A) : Selector[A] = copy[A](res = res.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = new Columns3[C1,C2,C](a1,a2,a)
}
final class Columns3[C1 : SQLType, C2 : SQLType, C3 : SQLType](
    a1 : SelectExpr[C1]
  , a2 : SelectExpr[C2]
  , a3 : SelectExpr[C3]
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3),SQLCols3[C1,C2,C3]](Seq(a1, a2, a3), from, new SQLCols3[C1,C2,C3]) {
  def map[A](f : (C1,C2,C3) => A) : Selector[A] = copy[A](res = res.map(f))
  def ~+[C : SQLType](a : SelectExpr[C]) = ???
}

object Columns {
  def apply(implicit from : FromTable) = new Columns0
  def apply[C1 : SQLType](a1 : SelectExpr[C1])(implicit from : FromTable) =
    new Columns1[C1](a1)
  def apply[C1 : SQLType, C2 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2])(implicit from : FromTable) =
    new Columns2[C1,C2](a1,a2)
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3])(implicit from : FromTable) =
    new Columns3[C1,C2,C3](a1,a2,a3)
}
