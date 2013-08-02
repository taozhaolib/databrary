package models

import play.api.mvc.{PathBindable,QueryStringBindable}
import anorm._
import dbrary._
import util._

class CachedVal[T, S](init : S => T) {
  private var x : Option[T] = None
  def apply(s : S) : T = synchronized(x.getOrElse(update(init(s))))
  def update(v : T) : T = {
    x = Some(v)
    v
  }
}
object CachedVal {
  import scala.language.implicitConversions
  def apply[T, S](init : S => T) = new CachedVal[T,S](init)
  implicit def implicitGetCached[T, S](x : CachedVal[T, S])(implicit s : S) : T = x(s)
}

class GenericId[I,+T](val unId : I) {
  // I don't understand why this is necessary (and it's also not quite right with inheritance):
  def equals(i : GenericId[I,_]) = i.unId equals unId
  def ==(i : GenericId[I,_]) = i.unId == unId
  def !=(i : GenericId[I,_]) = !(this == i)
  override def toString = "Id(" + unId.toString + ")"
}
final class IntId[+T](unId : Int) extends GenericId[Int,T](unId)
object IntId {
  def apply[T](i : Int) = new IntId[T](i)
  implicit def pathBindableId[T] : PathBindable[IntId[T]] = PathBindable.bindableInt.transform(apply[T] _, _.unId)
  implicit def queryStringBindableId[T] : QueryStringBindable[IntId[T]] = QueryStringBindable.bindableInt.transform(apply[T] _, _.unId)
  implicit def toStatementId[T] : ToStatement[IntId[T]] = dbrary.Anorm.toStatementMap[IntId[T],Int](_.unId)
  implicit def columnId[T] : Column[IntId[T]] = dbrary.Anorm.columnMap[IntId[T],Int](apply[T] _)
}
private[models] trait HasId[+T] {
  type Id = IntId[T]
  def asId(i : Int) : Id = new IntId[T](i)
}

private[models] abstract class Columns[+T](val cols : String*) extends RowParser[T] {
  val select = cols.mkString(", ")
}
private[models] sealed abstract class ColumnTuple[+T <: Product](cols : String*) extends Columns[T](cols : _*) {
  def ~+[C](a : String)(implicit c : Column[C]) : ColumnTuple[_]
}
private[models] object Columns {
  /* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
  def unit : Columns[Unit] = new Columns[Unit]() {
    def apply(row : Row) = Success(())
    def map[A](f : => A) : RowParser[A] = map[A]((_ : Unit) => f)
    def ~+[C1](a1 : String)(implicit c1 : Column[C1]) = tuple[C1](a1)(c1)
  }
  def tuple[C1](a1 : String)(implicit c1 : Column[C1]) : Columns[C1] = new Columns[C1](a1) {
    def apply(row : Row) = SqlParser.get[C1](a1)(c1)(row)
    def ~+[C2](a2 : String)(implicit c2 : Column[C2]) = tuple[C1,C2](a1,a2)(c1,c2)
  }
  def tuple[C1,C2](a1 : String, a2 : String)(implicit c1 : Column[C1], c2 : Column[C2]) = new ColumnTuple[(C1, C2)](a1, a2) {
    def apply(row : Row) = 
      for {
        r1 <- SqlParser.get[C1](a1)(c1)(row)
        r2 <- SqlParser.get[C2](a2)(c2)(row)
      } yield (r1, r2)
    def map[A](f : (C1, C2) => A) : RowParser[A] = map[A](f.tupled)
    def ~+[C3](a3 : String)(implicit c3 : Column[C3]) = tuple[C1,C2,C3](a1,a2,a3)(c1,c2,c3)
  }
  def tuple[C1,C2,C3](a1 : String, a2 : String, a3 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3]) = new ColumnTuple[(C1, C2, C3)](a1, a2, a3) {
    def apply(row : Row) = 
      for {
        r1 <- SqlParser.get[C1](a1)(c1)(row)
        r2 <- SqlParser.get[C2](a2)(c2)(row)
        r3 <- SqlParser.get[C3](a3)(c3)(row)
      } yield (r1, r2, r3)
    def map[A](f : (C1, C2, C3) => A) : RowParser[A] = map[A](f.tupled)
    def ~+[C4](a4 : String)(implicit c4 : Column[C4]) = tuple[C1,C2,C3,C4](a1,a2,a3,a4)(c1,c2,c3,c4)
  }
  def tuple[C1,C2,C3,C4](a1 : String, a2 : String, a3 : String, a4 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4]) = new ColumnTuple[(C1, C2, C3, C4)](a1, a2, a3, a4) {
    def apply(row : Row) = 
      for {
        r1 <- SqlParser.get[C1](a1)(c1)(row)
        r2 <- SqlParser.get[C2](a2)(c2)(row)
        r3 <- SqlParser.get[C3](a3)(c3)(row)
        r4 <- SqlParser.get[C4](a4)(c4)(row)
      } yield (r1, r2, r3, r4)
    def map[A](f : (C1, C2, C3, C4) => A) : RowParser[A] = map[A](f.tupled)
    def ~+[C5](a5 : String)(implicit c5 : Column[C5]) = tuple[C1,C2,C3,C4,C5](a1,a2,a3,a4,a5)(c1,c2,c3,c4,c5)
  }
  def tuple[C1,C2,C3,C4,C5](a1 : String, a2 : String, a3 : String, a4 : String, a5 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4], c5 : Column[C5]) = new ColumnTuple[(C1, C2, C3, C4, C5)](a1, a2, a3, a4, a5) {
    def apply(row : Row) = 
      for {
        r1 <- SqlParser.get[C1](a1)(c1)(row)
        r2 <- SqlParser.get[C2](a2)(c2)(row)
        r3 <- SqlParser.get[C3](a3)(c3)(row)
        r4 <- SqlParser.get[C4](a4)(c4)(row)
        r5 <- SqlParser.get[C5](a5)(c5)(row)
      } yield (r1, r2, r3, r4, r5)
    def map[A](f : (C1, C2, C3, C4, C5) => A) : RowParser[A] = map[A](f.tupled)
    def ~+[C6](a6 : String)(implicit c6 : Column[C6]) = tuple[C1,C2,C3,C4,C5,C6](a1,a2,a3,a4,a5,a6)(c1,c2,c3,c4,c5,c6)
  }
  def tuple[C1,C2,C3,C4,C5,C6](a1 : String, a2 : String, a3 : String, a4 : String, a5 : String, a6 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4], c5 : Column[C5], c6 : Column[C6]) = new ColumnTuple[(C1, C2, C3, C4, C5, C6)](a1, a2, a3, a4, a5, a6) {
    def apply(row : Row) = 
      for {
        r1 <- SqlParser.get[C1](a1)(c1)(row)
        r2 <- SqlParser.get[C2](a2)(c2)(row)
        r3 <- SqlParser.get[C3](a3)(c3)(row)
        r4 <- SqlParser.get[C4](a4)(c4)(row)
        r5 <- SqlParser.get[C5](a5)(c5)(row)
        r6 <- SqlParser.get[C6](a6)(c6)(row)
      } yield (r1, r2, r3, r4, r5, r6)
    def map[A](f : (C1, C2, C3, C4, C5, C6) => A) : RowParser[A] = map[A](f.tupled)
    def ~+[C7](a7 : String)(implicit c7 : Column[C7]) = ???
  }
}

private[models] trait TableRow
private[models] trait TableRowId[+T] extends TableRow {
  val id : IntId[T]
  override def hashCode = id.unId
  def equals(a : this.type) = a.id == id
}

private[models] abstract trait TableView[R <: TableRow] {
  private[models] val table : String
  private[this] val _tableOID = CachedVal[Long,Site.DB](SQL("SELECT oid FROM pg_class WHERE relname = {name}").on('name -> table).single(SqlParser.scalar[Long])(_))
  private[models] def tableOID(implicit db : Site.DB) : Long = _tableOID
  protected final def col(n : String*) : String = n.map(table + "." + _).mkString(", ")
  private[models] val columns : Columns[_]
  private[models] def * : String = columns.select
  private[models] val src : String = table
  protected def SELECT(q : String = "") = 
    SQL("SELECT " + * + " FROM " + src + (if (q.isEmpty) "" else " " + q))//.using(row)
  protected def JOIN(t : TableView[_], q : String = "") = 
    SQL("SELECT " + * + ", " + t.* + " FROM " + src + " JOIN " + t.src + (if (q.isEmpty) "" else " " + q))//.using(row ~ t.row)
}
private[models] abstract class Table[R <: TableRow](private[models] val table : String) extends TableView[R] {
  // private[models] val row : RowParser[R]
}

private[models] abstract class TableColumns1[R <: TableRow, C1](table : String, a1 : String)(implicit c1 : Column[C1]) extends Table[R](table) {
  private[models] val columns = Columns.tuple[C1](col(a1))(c1)
}
private[models] abstract class TableColumns2[R <: TableRow, C1, C2](table : String, a1 : String, a2 : String)(implicit c1 : Column[C1], c2 : Column[C2]) extends Table[R](table) {
  private[models] val columns = Columns.tuple[C1,C2](col(a1), col(a2))(c1, c2)
}
private[models] abstract class TableColumns3[R <: TableRow, C1, C2, C3](table : String, a1 : String, a2 : String, a3 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3]) extends Table[R](table) {
  private[models] val columns = Columns.tuple[C1,C2,C3](col(a1), col(a2), col(a3))(c1, c2, c3)
}
private[models] abstract class TableColumns4[R <: TableRow, C1, C2, C3, C4](table : String, a1 : String, a2 : String, a3 : String, a4 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4]) extends Table[R](table) {
  private[models] val columns = Columns.tuple[C1,C2,C3,C4](col(a1), col(a2), col(a3), col(a4))(c1, c2, c3, c4)
}
private[models] abstract class TableColumns5[R <: TableRow, C1, C2, C3, C4, C5](table : String, a1 : String, a2 : String, a3 : String, a4 : String, a5 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4], c5 : Column[C5]) extends Table[R](table) {
  private[models] val columns = Columns.tuple[C1,C2,C3,C4,C5](col(a1), col(a2), col(a3), col(a4), col(a5))(c1, c2, c3, c4, c5)
}
private[models] abstract class TableColumns6[R <: TableRow, C1, C2, C3, C4, C5, C6](table : String, a1 : String, a2 : String, a3 : String, a4 : String, a5 : String, a6 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4], c5 : Column[C5], c6 : Column[C6]) extends Table[R](table) {
  private[models] val columns = Columns.tuple[C1,C2,C3,C4,C5,C6](col(a1), col(a2), col(a3), col(a4), col(a5), col(a6))(c1, c2, c3, c4, c5, c6)
}
private[models] abstract class TableColumnsId1[R <: TableRowId[R], C1](table : String, a1 : String)(implicit c1 : Column[C1]) extends Table[R](table) with HasId[R] {
  private[models] val columns = Columns.tuple[Id,C1](col("id"), col(a1))(implicitly[Column[Id]], c1)
}
private[models] abstract class TableColumnsId2[R <: TableRowId[R], C1, C2](table : String, a1 : String, a2 : String)(implicit c1 : Column[C1], c2 : Column[C2]) extends Table[R](table) with HasId[R] {
  private[models] val columns = Columns.tuple[Id,C1,C2](col("id"), col(a1), col(a2))(implicitly[Column[Id]], c1, c2)
}
private[models] abstract class TableColumnsId3[R <: TableRowId[R], C1, C2, C3](table : String, a1 : String, a2 : String, a3 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3]) extends Table[R](table) with HasId[R] {
  private[models] val columns = Columns.tuple[Id,C1,C2,C3](col("id"), col(a1), col(a2), col(a3))(implicitly[Column[Id]], c1, c2, c3)
}
private[models] abstract class TableColumnsId4[R <: TableRowId[R], C1, C2, C3, C4](table : String, a1 : String, a2 : String, a3 : String, a4 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4]) extends Table[R](table) with HasId[R] {
  private[models] val columns = Columns.tuple[Id,C1,C2,C3,C4](col("id"), col(a1), col(a2), col(a3), col(a4))(implicitly[Column[Id]], c1, c2, c3, c4)
}
private[models] abstract class TableColumnsId5[R <: TableRowId[R], C1, C2, C3, C4, C5](table : String, a1 : String, a2 : String, a3 : String, a4 : String, a5 : String)(implicit c1 : Column[C1], c2 : Column[C2], c3 : Column[C3], c4 : Column[C4], c5 : Column[C5]) extends Table[R](table) with HasId[R] {
  private[models] val columns = Columns.tuple[Id,C1,C2,C3,C4,C5](col("id"), col(a1), col(a2), col(a3), col(a4), col(a5))(implicitly[Column[Id]], c1, c2, c3, c4, c5)
}

private[models] object Anorm {
  type Args = Seq[(Symbol, ParameterValue[_])]
  def Args(args : (Symbol, ParameterValue[_])*) : Args = List(args : _*)

  def insertArgs(args : Args) = {
    val names = args.map(_._1.name)
    names.mkString("(", ", ", ")") + " VALUES " + names.mkString("({", "}, {", "})")
  }

  def setArgs(args : Args, sep : String = ", ") =
    args.map(_._1.name).map(n => n + " = {" + n + "}").mkString(sep)
}

trait SitePage {
  def pageName(implicit site : Site) : String
  def pageParent(implicit site : Site) : Option[SitePage]
  def pageURL : String
}
