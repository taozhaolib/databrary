package dbrary

import anorm._

sealed trait Select {
  def toString : String
  def get[T](implicit c : Column[T]) : RowParser[T]
  def inTable(table : String) : Select
}

case class SelectColumn(table : String, col : String) extends Select {
  override def toString = table + "." + col
  def get[T : Column] : RowParser[T] = SqlParser.get[T](toString)
  def inTable(table : String) = copy(table = table)
}

case class SelectAs(expr : String, name : String) extends Select {
  override def toString = expr + " AS " + name
  override def get[T : Column] : RowParser[T] = SqlParser.getAliased[T](name)
  def inTable(table : String) = this
}

trait Selects {
  val selects : Seq[Select]
  def select = selects.mkString(", ")
}

object SelectParser {
  def apply[A](s : Seq[Select], f : Row => SqlResult[A]) : SelectParser[A] = new SelectParser[A] {
    val selects = s
    def apply(row : Row) = f(row)
  }
}

trait SelectParser[+A] extends RowParser[A] with Selects {
  private[this] def copy[B](f : RowParser[B]) : SelectParser[B] = SelectParser[B](selects, f)
  override def map[B](f : A => B) : SelectParser[B] = copy(super.map[B](f))
  override def ? : SelectParser[Option[A]] = copy(super.?)
  def ~[B](p : SelectParser[B]) : SelectParser[A ~ B] =
    SelectParser[A ~ B](selects ++ p.selects, super.~[B](p))
  def inTable(table : String) : SelectParser[A] =
    SelectParser[A](selects.map(_.inTable(table)), this)
}

sealed abstract class Columns[+A](cols : Select*) extends SelectParser[A] {
  val selects = cols
  def ~+[C : Column](a : Select) : Columns[_]
}

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
object Columns0 extends Columns[Unit]() {
  def apply(row : Row) = Success(())
  def map[A](f : => A) : SelectParser[A] = map[A]((_ : Unit) => f)
  def ~+[C1 : Column](a1 : Select) = Columns[C1](a1)
}
final class Columns1[C1 : Column](
    a1 : Select
  ) extends Columns[C1](a1) {
  def apply(row : Row) = a1.get[C1].apply(row)
  def ~+[C2 : Column](a2 : Select) = Columns[C1,C2](a1,a2)
}
final class Columns2[C1 : Column, C2 : Column](
    a1 : Select
  , a2 : Select
  ) extends Columns[(C1,C2)](a1,a2) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
    } yield (r1, r2)
  def map[A](f : (C1, C2) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C3 : Column](a3 : Select) = Columns[C1,C2,C3](a1,a2,a3)
}
final class Columns3[C1 : Column, C2 : Column, C3 : Column](
    a1 : Select
  , a2 : Select
  , a3 : Select
  ) extends Columns[(C1,C2,C3)](a1,a2,a3) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
    } yield (r1, r2, r3)
  def map[A](f : (C1, C2, C3) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C4 : Column](a4 : Select) = Columns[C1,C2,C3,C4](a1,a2,a3,a4)
}
final class Columns4[C1 : Column, C2 : Column, C3 : Column, C4 : Column](
    a1 : Select
  , a2 : Select
  , a3 : Select
  , a4 : Select
  ) extends Columns[(C1,C2,C3,C4)](a1,a2,a3,a4) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
      r4 <- a4.get[C4].apply(row)
    } yield (r1, r2, r3, r4)
  def map[A](f : (C1, C2, C3, C4) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C5 : Column](a5 : Select) = Columns[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)
}
final class Columns5[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column](
    a1 : Select
  , a2 : Select
  , a3 : Select
  , a4 : Select
  , a5 : Select
  ) extends Columns[(C1,C2,C3,C4,C5)](a1, a2, a3, a4, a5) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
      r4 <- a4.get[C4].apply(row)
      r5 <- a5.get[C5].apply(row)
    } yield (r1, r2, r3, r4, r5)
  def map[A](f : (C1, C2, C3, C4, C5) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C6 : Column](a6 : Select) = Columns[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)
}
final class Columns6[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column, C6 : Column](
    a1 : Select
  , a2 : Select
  , a3 : Select
  , a4 : Select
  , a5 : Select
  , a6 : Select
  ) extends Columns[(C1,C2,C3,C4,C5,C6)](a1, a2, a3, a4, a5, a6) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
      r4 <- a4.get[C4].apply(row)
      r5 <- a5.get[C5].apply(row)
      r6 <- a6.get[C6].apply(row)
    } yield (r1, r2, r3, r4, r5, r6)
  def map[A](f : (C1, C2, C3, C4, C5, C6) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C7 : Column](a7 : Select) = ???
}

object Columns {
  def apply : Columns0.type = Columns0
  def apply[C1 : Column](a1 : Select) = new Columns1[C1](a1)
  def apply[C1 : Column, C2 : Column](a1 : Select, a2 : Select) = new Columns2[C1,C2](a1,a2)
  def apply[C1 : Column, C2 : Column, C3 : Column](a1 : Select, a2 : Select, a3 : Select) = new Columns3[C1,C2,C3](a1,a2,a3)
  def apply[C1 : Column, C2 : Column, C3 : Column, C4 : Column](a1 : Select, a2 : Select, a3 : Select, a4 : Select) = new Columns4[C1,C2,C3,C4](a1,a2,a3,a4)
  def apply[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column](a1 : Select, a2 : Select, a3 : Select, a4 : Select, a5 : Select) = new Columns5[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)
  def apply[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column, C6 : Column](a1 : Select, a2 : Select, a3 : Select, a4 : Select, a5 : Select, a6 : Select) = new Columns6[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)
}
