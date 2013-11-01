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

final case class Selector[A](selects : Seq[SelectExpr[_]], source : String, res : SQLResub[A]) {
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

  def SELECT(q : String*)(implicit dbc : db.Connection, executionContext : ExecutionContext) : Future[SQLResult[A]] =
    res(dbc.sendQuery(unwords(Seq("SELECT", select, "FROM", source) ++ q : _*)))
}

object Columns {
  def apply(implicit from : FromTable) : Selector[Unit] =
    Selector(Nil, from.table, SQLCols.empty)
  def apply[C1 : SQLType](a1 : SelectExpr[C1])(implicit from : FromTable) : Selector[C1] =
    Selector(Nil, from.table, SQLCols[C1])
  def apply[C1 : SQLType, C2 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2])(implicit from : FromTable) : Selector[(C1,C2)] =
    Selector(Nil, from.table, SQLCols[C1,C2])
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType](a1 : SelectExpr[C1], a2 : SelectExpr[C2], a3 : SelectExpr[C3])(implicit from : FromTable) : Selector[(C1,C2,C3)] =
    Selector(Nil, from.table, SQLCols[C1,C2,C3])
}
