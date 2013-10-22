package dbrary

import anorm._

case class FromTable(table : String) {
  implicit override def toString : String = table
}

sealed trait SelectExpr {
  def toString : String
  def get[T](implicit c : Column[T]) : RowParser[T]
  def inTable(table : String) : SelectExpr = this
}
object SelectExpr {
  def apply(s : String) = new SelectExpr {
    override def toString = s
    def get[T : Column] : RowParser[T] = SqlParser.get[T](toString)
  }
}

case class SelectColumn(table : String, col : String) extends SelectExpr {
  override def toString = table + "." + col
  def get[T : Column] : RowParser[T] = SqlParser.get[T](toString)
  override def inTable(table : String) = copy(table = table)
}
object SelectColumn {
  def apply(col : String)(implicit from : FromTable) : SelectColumn = SelectColumn(from.table, col)
}

case class SelectAs(expr : String, name : String) extends SelectExpr {
  override def toString = expr + " AS " + name
  override def get[T : Column] : RowParser[T] = SqlParser.getAliased[T](name)
}

trait SelectExprs {
  val selects : Seq[SelectExpr]
  def select = selects.mkString(", ")
}

trait Selector[A] extends RowParser[A] with SelectExprs {
  val source : String
  override def map[B](f : A => B) : Selector[B] =
    Selector[B](selects, source, super.map[B](f))
  override def ? : Selector[Option[A]] =
    Selector[Option[A]](selects, source, super.?)
  def from(from : String) : Selector[A] =
    Selector[A](selects, from, this)

  def join[B](that : Selector[B], joiner : (String, String) => String) : Selector[A ~ B] =
    Selector(selects ++ that.selects, joiner(source, that.source), super.~[B](that))
  def join[B](that : Selector[B], on : String) : Selector[A ~ B] =
    join(that, unwords(_, "JOIN", _, "ON", on))
  def join[B](that : Selector[B], using : Seq[String]) : Selector[A ~ B] =
    join(that, unwords(_, "JOIN", _, "USING", using.mkString("(", ", ", ")")))
  def join[B](that : Selector[B], using : Symbol) : Selector[A ~ B] =
    join(that, Seq(using.name))
  def leftJoin[B](that : Selector[B], on : String) : Selector[A ~ Option[B]] =
    join(that.?, unwords(_, "LEFT JOIN", _, "ON", on))
  def leftJoin[B](that : Selector[B], using : Seq[String]) : Selector[A ~ Option[B]] =
    join(that.?, unwords(_, "LEFT JOIN", _, "USING", using.mkString("(", ", ", ")")))
  def leftJoin[B](that : Selector[B], using : Symbol) : Selector[A ~ Option[B]] =
    leftJoin(that, Seq(using.name))
  def ~[B](that : Selector[B]) : Selector[A ~ B] =
    join(that, _ + " NATURAL JOIN " + _)

  def SQL(q : String*) : SimpleSql[A] =
    anorm.SQL(unwords(Seq("SELECT", select, "FROM", source) ++ q : _*)).using(this)
}
object Selector {
  def apply[A](sel : Seq[SelectExpr], src : String, parse : Row => SqlResult[A]) : Selector[A] = new Selector[A] {
    val selects = sel
    val source = src
    def apply(row : Row) = parse(row)
  }
}

sealed abstract class Columns[A](cols : SelectExpr*)(implicit from : FromTable) extends Selector[A] {
  val selects = cols
  val source = from.table
  /** Append a new column.  This resets any map and from. */
  def ~+[C : Column](a : SelectExpr) : Columns[_]
}

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
final class Columns0(implicit from : FromTable) extends Columns[Unit]() {
  def apply(row : Row) = Success(())
  def map[A](f : => A) : Selector[A] = map[A]((_ : Unit) => f)
  def ~+[C1 : Column](a1 : SelectExpr) = new Columns1[C1](a1)
}
final class Columns1[C1 : Column](
    a1 : SelectExpr
  )(implicit from : FromTable)
  extends Columns[C1](a1) {
  def apply(row : Row) = a1.get[C1].apply(row)
  def ~+[C2 : Column](a2 : SelectExpr) = new Columns2[C1,C2](a1,a2)
}
final class Columns2[C1 : Column, C2 : Column](
    a1 : SelectExpr
  , a2 : SelectExpr
  )(implicit from : FromTable)
  extends Columns[(C1,C2)](a1,a2) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
    } yield (r1, r2)
  def map[A](f : (C1, C2) => A) : Selector[A] = map[A](f.tupled)
  def ~+[C3 : Column](a3 : SelectExpr) = new Columns3[C1,C2,C3](a1,a2,a3)
}
final class Columns3[C1 : Column, C2 : Column, C3 : Column](
    a1 : SelectExpr
  , a2 : SelectExpr
  , a3 : SelectExpr
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3)](a1,a2,a3) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
    } yield (r1, r2, r3)
  def map[A](f : (C1, C2, C3) => A) : Selector[A] = map[A](f.tupled)
  def ~+[C4 : Column](a4 : SelectExpr) = new Columns4[C1,C2,C3,C4](a1,a2,a3,a4)
}
final class Columns4[C1 : Column, C2 : Column, C3 : Column, C4 : Column](
    a1 : SelectExpr
  , a2 : SelectExpr
  , a3 : SelectExpr
  , a4 : SelectExpr
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4)](a1,a2,a3,a4) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
      r4 <- a4.get[C4].apply(row)
    } yield (r1, r2, r3, r4)
  def map[A](f : (C1, C2, C3, C4) => A) : Selector[A] = map[A](f.tupled)
  def ~+[C5 : Column](a5 : SelectExpr) = new Columns5[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)
}
final class Columns5[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column](
    a1 : SelectExpr
  , a2 : SelectExpr
  , a3 : SelectExpr
  , a4 : SelectExpr
  , a5 : SelectExpr
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5)](a1, a2, a3, a4, a5) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
      r4 <- a4.get[C4].apply(row)
      r5 <- a5.get[C5].apply(row)
    } yield (r1, r2, r3, r4, r5)
  def map[A](f : (C1, C2, C3, C4, C5) => A) : Selector[A] = map[A](f.tupled)
  def ~+[C6 : Column](a6 : SelectExpr) = new Columns6[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)
}
final class Columns6[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column, C6 : Column](
    a1 : SelectExpr
  , a2 : SelectExpr
  , a3 : SelectExpr
  , a4 : SelectExpr
  , a5 : SelectExpr
  , a6 : SelectExpr
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5,C6)](a1, a2, a3, a4, a5, a6) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
      r4 <- a4.get[C4].apply(row)
      r5 <- a5.get[C5].apply(row)
      r6 <- a6.get[C6].apply(row)
    } yield (r1, r2, r3, r4, r5, r6)
  def map[A](f : (C1, C2, C3, C4, C5, C6) => A) : Selector[A] = map[A](f.tupled)
  def ~+[C7 : Column](a7 : SelectExpr) = new Columns7[C1,C2,C3,C4,C5,C6,C7](a1,a2,a3,a4,a5,a6,a7)
}
final class Columns7[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column, C6 : Column, C7 : Column](
    a1 : SelectExpr
  , a2 : SelectExpr
  , a3 : SelectExpr
  , a4 : SelectExpr
  , a5 : SelectExpr
  , a6 : SelectExpr
  , a7 : SelectExpr
  )(implicit from : FromTable)
  extends Columns[(C1,C2,C3,C4,C5,C6,C7)](a1, a2, a3, a4, a5, a6, a7) {
  def apply(row : Row) = 
    for {
      r1 <- a1.get[C1].apply(row)
      r2 <- a2.get[C2].apply(row)
      r3 <- a3.get[C3].apply(row)
      r4 <- a4.get[C4].apply(row)
      r5 <- a5.get[C5].apply(row)
      r6 <- a6.get[C6].apply(row)
      r7 <- a7.get[C7].apply(row)
    } yield (r1, r2, r3, r4, r5, r6, r7)
  def map[A](f : (C1, C2, C3, C4, C5, C6, C7) => A) : Selector[A] = map[A](f.tupled)
  def ~+[C8 : Column](a8 : SelectExpr) = ???
}

object Columns {
  def apply(implicit from : FromTable) = new Columns0
  def apply[C1 : Column](a1 : SelectExpr)(implicit from : FromTable) = new Columns1[C1](a1)
  def apply[C1 : Column, C2 : Column](a1 : SelectExpr, a2 : SelectExpr)(implicit from : FromTable) = new Columns2[C1,C2](a1,a2)
  def apply[C1 : Column, C2 : Column, C3 : Column](a1 : SelectExpr, a2 : SelectExpr, a3 : SelectExpr)(implicit from : FromTable) = new Columns3[C1,C2,C3](a1,a2,a3)
  def apply[C1 : Column, C2 : Column, C3 : Column, C4 : Column](a1 : SelectExpr, a2 : SelectExpr, a3 : SelectExpr, a4 : SelectExpr)(implicit from : FromTable) = new Columns4[C1,C2,C3,C4](a1,a2,a3,a4)
  def apply[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column](a1 : SelectExpr, a2 : SelectExpr, a3 : SelectExpr, a4 : SelectExpr, a5 : SelectExpr)(implicit from : FromTable) = new Columns5[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)
  def apply[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column, C6 : Column](a1 : SelectExpr, a2 : SelectExpr, a3 : SelectExpr, a4 : SelectExpr, a5 : SelectExpr, a6 : SelectExpr)(implicit from : FromTable) = new Columns6[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)
  def apply[C1 : Column, C2 : Column, C3 : Column, C4 : Column, C5 : Column, C6 : Column, C7 : Column](a1 : SelectExpr, a2 : SelectExpr, a3 : SelectExpr, a4 : SelectExpr, a5 : SelectExpr, a6 : SelectExpr, a7 : SelectExpr)(implicit from : FromTable) = new Columns7[C1,C2,C3,C4,C5,C6,C7](a1,a2,a3,a4,a5,a6,a7)
}
