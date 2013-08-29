package dbrary

import anorm._

sealed trait Select {
  def toString : String
  def get[T](implicit c : Column[T]) : RowParser[T]
  def inTable(table : String) : Select
}

case class SelectColumn(table : String, col : String) extends Select {
  override def toString = table + "." + col
  def get[T](implicit c : Column[T]) : RowParser[T] = SqlParser.get[T](toString)(c)
  def inTable(table : String) = copy(table = table)
}

case class SelectAs(expr : String, name : String) extends Select {
  override def toString = expr + " AS " + name
  override def get[T](implicit c : Column[T]) : RowParser[T] = SqlParser.getAliased[T](name)(c)
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
  def ~+[C](a : Select)(implicit c : Column[C]) : Columns[_]
}

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
object Columns0 extends Columns[Unit]() {
  def apply(row : Row) = Success(())
  def map[A](f : => A) : SelectParser[A] = map[A]((_ : Unit) => f)
  def ~+[C1](a1 : Select)(implicit c1 : Column[C1]) = Columns[C1](a1)(c1)
}
final class Columns1[C1](
    a1 : Select
  )(implicit 
    c1 : Column[C1]
  ) extends Columns[C1](a1) {
  def apply(row : Row) = a1.get[C1](c1)(row)
  def ~+[C2](a2 : Select)(implicit c2 : Column[C2]) = Columns[C1,C2](a1,a2)(c1,c2)
}
final class Columns2[C1,C2](
    a1 : Select
  , a2 : Select
  )(implicit 
    c1 : Column[C1]
  , c2 : Column[C2]
  ) extends Columns[(C1,C2)](a1,a2) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1](c1)(row)
      r2 <- a2.get[C2](c2)(row)
    } yield (r1, r2)
  def map[A](f : (C1, C2) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C3](a3 : Select)(implicit c3 : Column[C3]) = Columns[C1,C2,C3](a1,a2,a3)(c1,c2,c3)
}
final class Columns3[C1,C2,C3](
    a1 : Select
  , a2 : Select
  , a3 : Select
  )(implicit 
    c1 : Column[C1]
  , c2 : Column[C2]
  , c3 : Column[C3]
  ) extends Columns[(C1,C2,C3)](a1,a2,a3) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1](c1)(row)
      r2 <- a2.get[C2](c2)(row)
      r3 <- a3.get[C3](c3)(row)
    } yield (r1, r2, r3)
  def map[A](f : (C1, C2, C3) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C4](a4 : Select)(implicit c4 : Column[C4]) = Columns[C1,C2,C3,C4](a1,a2,a3,a4)(c1,c2,c3,c4)
}
final class Columns4[C1,C2,C3,C4](
    a1 : Select
  , a2 : Select
  , a3 : Select
  , a4 : Select
  )(implicit 
    c1 : Column[C1]
  , c2 : Column[C2]
  , c3 : Column[C3]
  , c4 : Column[C4]
  ) extends Columns[(C1,C2,C3,C4)](a1,a2,a3,a4) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1](c1)(row)
      r2 <- a2.get[C2](c2)(row)
      r3 <- a3.get[C3](c3)(row)
      r4 <- a4.get[C4](c4)(row)
    } yield (r1, r2, r3, r4)
  def map[A](f : (C1, C2, C3, C4) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C5](a5 : Select)(implicit c5 : Column[C5]) = Columns[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)(c1,c2,c3,c4,c5)
}
final class Columns5[C1,C2,C3,C4,C5](
    a1 : Select
  , a2 : Select
  , a3 : Select
  , a4 : Select
  , a5 : Select
  )(implicit
    c1 : Column[C1]
  , c2 : Column[C2]
  , c3 : Column[C3]
  , c4 : Column[C4]
  , c5 : Column[C5]
  ) extends Columns[(C1,C2,C3,C4,C5)](a1, a2, a3, a4, a5) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1](c1)(row)
      r2 <- a2.get[C2](c2)(row)
      r3 <- a3.get[C3](c3)(row)
      r4 <- a4.get[C4](c4)(row)
      r5 <- a5.get[C5](c5)(row)
    } yield (r1, r2, r3, r4, r5)
  def map[A](f : (C1, C2, C3, C4, C5) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C6](a6 : Select)(implicit c6 : Column[C6]) = Columns[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)(c1,c2,c3,c4,c5,c6)
}
final class Columns6[C1,C2,C3,C4,C5,C6](
    a1 : Select
  , a2 : Select
  , a3 : Select
  , a4 : Select
  , a5 : Select
  , a6 : Select
  )(implicit
    c1 : Column[C1]
  , c2 : Column[C2]
  , c3 : Column[C3]
  , c4 : Column[C4]
  , c5 : Column[C5]
  , c6 : Column[C6]
  ) extends Columns[(C1,C2,C3,C4,C5,C6)](a1, a2, a3, a4, a5, a6) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1](c1)(row)
      r2 <- a2.get[C2](c2)(row)
      r3 <- a3.get[C3](c3)(row)
      r4 <- a4.get[C4](c4)(row)
      r5 <- a5.get[C5](c5)(row)
      r6 <- a6.get[C6](c6)(row)
    } yield (r1, r2, r3, r4, r5, r6)
  def map[A](f : (C1, C2, C3, C4, C5, C6) => A) : SelectParser[A] = map[A](f.tupled)
  def ~+[C7](a7 : Select)(implicit c7 : Column[C7]) = ???
}

object Columns {
  def apply : Columns0.type = Columns0
  def apply[C1](a1 : Select)(implicit c1 : Column[C1]) = new Columns1[C1](a1)(c1)
  def apply[C1,C2](a1 : Select, a2 : Select)(implicit c1 : Column[C1], c2 : Column[C2]) = new Columns2[C1,C2](a1,a2)(c1,c2)
  def apply[C1,C2,C3](a1 : Select, a2 : Select, a3 : Select)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3]) = new Columns3[C1,C2,C3](a1,a2,a3)(c1,c2,c3)
  def apply[C1,C2,C3,C4](a1 : Select, a2 : Select, a3 : Select, a4 : Select)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4]) = new Columns4[C1,C2,C3,C4](a1,a2,a3,a4)(c1,c2,c3,c4)
  def apply[C1,C2,C3,C4,C5](a1 : Select, a2 : Select, a3 : Select, a4 : Select, a5 : Select)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4], c5 : Column[C5]) = new Columns5[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)(c1,c2,c3,c4,c5)
  def apply[C1,C2,C3,C4,C5,C6](a1 : Select, a2 : Select, a3 : Select, a4 : Select, a5 : Select, a6 : Select)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4], c5 : Column[C5], c6 : Column[C6]) = new Columns6[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)(c1,c2,c3,c4,c5,c6)
}
