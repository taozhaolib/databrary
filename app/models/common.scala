package models

import play.api.mvc.PathBindable
import anorm._
import dbrary._

class CachedVal[T <: AnyRef, S](init : S => T) {
  private var x : Option[T] = None
  def apply(s : S) : T = x.getOrElse(update(init(s)))
  def update(v : T) : T = {
    x = Some(v)
    v
  }
}

object CachedVal {
  import scala.language.implicitConversions
  def apply[T <: AnyRef, S](init : S => T) = new CachedVal(init)
  implicit def implicitGetCached[T <: AnyRef, S](x : CachedVal[T, S])(implicit s : S) : T = x(s)
}

private[models] abstract trait TableRow
private[models] abstract class TableView[R <: TableRow](private[models] val table : String) {
  private[models] val row : RowParser[R]
  private[models] val * = "*"
}
private[models] abstract class TableViewId[R <: TableRow](table : String) extends TableView[R](table) {
  class Id private[TableViewId] (val unId : Int)
  def asId(i : Int) : Id = new Id(i)
  implicit val pathBindableId : PathBindable[Id] = implicitly[PathBindable[Int]].transform(asId _, _.unId)
  implicit val toStatementId : ToStatement[Id] = dbrary.Anorm.toStatementMap[Id,Int](_.unId)
  implicit val columnId : Column[Id] = dbrary.Anorm.columnMap[Id,Int](asId _)
}

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
      c1 : anorm.Column[C1], 
      c2 : anorm.Column[C2]
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
      c1 : anorm.Column[C1], 
      c2 : anorm.Column[C2],
      c3 : anorm.Column[C3]
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
      c1 : anorm.Column[C1], 
      c2 : anorm.Column[C2],
      c3 : anorm.Column[C3],
      c4 : anorm.Column[C4]
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
      c1 : anorm.Column[C1], 
      c2 : anorm.Column[C2],
      c3 : anorm.Column[C3],
      c4 : anorm.Column[C4],
      c5 : anorm.Column[C5]
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
      c1 : anorm.Column[C1], 
      c2 : anorm.Column[C2],
      c3 : anorm.Column[C3],
      c4 : anorm.Column[C4],
      c5 : anorm.Column[C5],
      c6 : anorm.Column[C6]
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
}
