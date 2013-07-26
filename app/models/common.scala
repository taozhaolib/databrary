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

private[models] trait TableRow
private[models] trait TableRowId[+T] extends TableRow {
  val id : IntId[T]
  override def hashCode = id.unId
  def equals(a : this.type) = a.id == id
}

private[models] abstract class TableView[R <: TableRow](private[models] val table : String) {
  private[this] val _tableOID = CachedVal[Long,Site.DB](SQL("SELECT oid FROM pg_class WHERE relname = {name}").on('name -> table).single(SqlParser.scalar[Long])(_))
  private[models] def tableOID(implicit db : Site.DB) : Long = _tableOID
  private[models] val row : RowParser[R]
  protected final def col(n : String*) : String = n.map(table + "." + _).mkString(", ")
  private[models] val * : String = col("*")
  private[models] val src : String = table
}
private[models] abstract class TableViewId[R <: TableRowId[R]](table : String) extends TableView[R](table) with HasId[R]

object Anorm {
  type Args = Seq[(Symbol, ParameterValue[_])]
  def Args(args : (Symbol, ParameterValue[_])*) : Args = List(args : _*)

  def insertArgs(args : Args) = {
    val names = args.map(_._1.name)
    names.mkString("(", ", ", ")") + " VALUES " + names.mkString("({", "}, {", "})")
  }

  def setArgs(args : Args, sep : String = ", ") =
    args.map(_._1.name).map(n => n + " = {" + n + "}").mkString(sep)

  /* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
  def rowMap[C1,B](f : (C1) => B
    , a1 : String
    )(implicit 
      c1: anorm.Column[C1]
    ) : RowParser[B] = RowParser[B] { row =>
    (for {
      r1 <- row.get[C1](a1)(c1)
    } yield f(r1)).fold(e => Error(e), b => Success(b))
  }
  def rowMap[C1,C2,B](f : (C1,C2) => B
    , a1 : String
    , a2 : String
    )(implicit 
      c1 : anorm.Column[C1]
    , c2 : anorm.Column[C2]
    ) : RowParser[B] = RowParser[B] { row =>
    (for {
      r1 <- row.get[C1](a1)(c1)
      r2 <- row.get[C2](a2)(c2)
    } yield f(r1, r2)).fold(e => Error(e), b => Success(b))
  }
  def rowMap[C1,C2,C3,B](f : (C1,C2,C3) => B
    , a1 : String
    , a2 : String
    , a3 : String
    )(implicit 
      c1 : anorm.Column[C1] 
    , c2 : anorm.Column[C2]
    , c3 : anorm.Column[C3]
    ) : RowParser[B] = RowParser[B] { row =>
    (for {
      r1 <- row.get[C1](a1)(c1)
      r2 <- row.get[C2](a2)(c2)
      r3 <- row.get[C3](a3)(c3)
    } yield f(r1, r2, r3)).fold(e => Error(e), b => Success(b))
  }
  def rowMap[C1,C2,C3,C4,B](f : (C1,C2,C3,C4) => B
    , a1 : String
    , a2 : String
    , a3 : String
    , a4 : String
    )(implicit 
      c1 : anorm.Column[C1] 
    , c2 : anorm.Column[C2]
    , c3 : anorm.Column[C3]
    , c4 : anorm.Column[C4]
    ) : RowParser[B] = RowParser[B] { row =>
    (for {
      r1 <- row.get[C1](a1)(c1)
      r2 <- row.get[C2](a2)(c2)
      r3 <- row.get[C3](a3)(c3)
      r4 <- row.get[C4](a4)(c4)
    } yield f(r1, r2, r3, r4)).fold(e => Error(e), b => Success(b))
  }
  def rowMap[C1,C2,C3,C4,C5,B](f : (C1,C2,C3,C4,C5) => B
    , a1 : String
    , a2 : String
    , a3 : String
    , a4 : String
    , a5 : String
    )(implicit 
      c1 : anorm.Column[C1] 
    , c2 : anorm.Column[C2]
    , c3 : anorm.Column[C3]
    , c4 : anorm.Column[C4]
    , c5 : anorm.Column[C5]
    ) : RowParser[B] = RowParser[B] { row =>
    (for {
      r1 <- row.get[C1](a1)(c1)
      r2 <- row.get[C2](a2)(c2)
      r3 <- row.get[C3](a3)(c3)
      r4 <- row.get[C4](a4)(c4)
      r5 <- row.get[C5](a5)(c5)
    } yield f(r1, r2, r3, r4, r5)).fold(e => Error(e), b => Success(b))
  }
  def rowMap[C1,C2,C3,C4,C5,C6,B](f : (C1,C2,C3,C4,C5,C6) => B
    , a1 : String
    , a2 : String
    , a3 : String
    , a4 : String
    , a5 : String
    , a6 : String
    )(implicit 
      c1 : anorm.Column[C1] 
    , c2 : anorm.Column[C2]
    , c3 : anorm.Column[C3]
    , c4 : anorm.Column[C4]
    , c5 : anorm.Column[C5]
    , c6 : anorm.Column[C6]
    ) : RowParser[B] = RowParser[B] { row =>
    (for {
      r1 <- row.get[C1](a1)(c1)
      r2 <- row.get[C2](a2)(c2)
      r3 <- row.get[C3](a3)(c3)
      r4 <- row.get[C4](a4)(c4)
      r5 <- row.get[C5](a5)(c5)
      r6 <- row.get[C6](a6)(c6)
    } yield f(r1, r2, r3, r4, r5, r6)).fold(e => Error(e), b => Success(b))
  }
  def rowMap[C1,C2,C3,C4,C5,C6,C7,B](f : (C1,C2,C3,C4,C5,C6,C7) => B
    , a1 : String
    , a2 : String
    , a3 : String
    , a4 : String
    , a5 : String
    , a6 : String
    , a7 : String
    )(implicit 
      c1 : anorm.Column[C1] 
    , c2 : anorm.Column[C2]
    , c3 : anorm.Column[C3]
    , c4 : anorm.Column[C4]
    , c5 : anorm.Column[C5]
    , c6 : anorm.Column[C6]
    , c7 : anorm.Column[C7]
    ) : RowParser[B] = RowParser[B] { row =>
    (for {
      r1 <- row.get[C1](a1)(c1)
      r2 <- row.get[C2](a2)(c2)
      r3 <- row.get[C3](a3)(c3)
      r4 <- row.get[C4](a4)(c4)
      r5 <- row.get[C5](a5)(c5)
      r6 <- row.get[C6](a6)(c6)
      r7 <- row.get[C7](a7)(c7)
    } yield f(r1, r2, r3, r4, r5, r6, r7)).fold(e => Error(e), b => Success(b))
  }
}

trait SitePage {
  def pageName(implicit site : Site) : String
  def pageParent(implicit site : Site) : Option[SitePage]
  def pageURL : String
}
