package dbrary.SQL

import scala.concurrent.{Future,ExecutionContext}
import scala.util.control.Exception.catching
import com.github.mauricio.async.db
import macros._

/** Exceptions that may occur during parsing query results, usually indicating type or arity mismatches. */
class ResultException(message : String) extends db.exceptions.DatabaseException(message)

/** A simple wrapper for Future[QueryResult] representing the result of a query. */
class Result(val result : Future[db.QueryResult])(implicit context : ExecutionContext) {
  private[this] def fail(res : db.QueryResult, msg : String) = throw new ResultException(res.statusMessage + ": " + msg)
  private[this] def rows(res : db.QueryResult) = res.rows.getOrElse(fail(res, "no results"))
  private[this] def singleOpt(res : db.QueryResult) = rows(res) match {
    case Seq() => None
    case Seq(r) => Some(r)
    case l => fail(res, "got " + l.length + " rows, expected 1")
  }
  private[this] def single(res : db.QueryResult) = singleOpt(res).getOrElse(fail(res, "got no rows, expected 1"))

  def future(f : Future[db.QueryResult] => Future[db.QueryResult]) : Result =
    new Result(f(result))
  def map[A](f : db.QueryResult => A) : Future[A] = result.map(f)
  def flatMap[A](f : db.QueryResult => Future[A]) : Future[A] = result.flatMap(f)
  def rowsAffected : Future[Long] = map(_.rowsAffected)
  def execute : Future[Boolean] = map(_.rowsAffected > 0)
  def ensure : Future[Unit] = map(r => if (r.rowsAffected > 0) () else fail(r, "no rows affected"))

  def as[A](parse : Row[A]) : Rows[A] = new Rows[A](result, parse)

  def list[A](parse : Row[A]) : Future[IndexedSeq[A]] = map(rows(_).map(parse(_)))
  def singleOpt[A](parse : Row[A]) : Future[Option[A]] = map(singleOpt(_).map(parse(_)))
  def single[A](parse : Row[A]) : Future[A] = map(r => parse(single(r)))
}

object Result {
  private final val emptyResult = new db.QueryResult(1, "empty") // 1 to emulate "success"
  def empty(implicit context : ExecutionContext) = new Result(async(emptyResult))(context)
}

/** Result with an associated row parser. */
final class Rows[+A](result : Future[db.QueryResult], parse : Row[A])(implicit context : ExecutionContext) extends Result(result)(context) {
  override def future(f : Future[db.QueryResult] => Future[db.QueryResult]) : Rows[A] =
    new Rows[A](f(result), parse)
  def map[B](f : A => B) : Rows[B] = new Rows[B](result, parse.map[B](f))
  def list : Future[IndexedSeq[A]] = list(parse)
  def singleOpt : Future[Option[A]] = singleOpt(parse)
  def single : Future[A] = single(parse)
}

/** A generic row parser that can transform RowData query results. */
trait Row[+A] extends (db.RowData => A) {
  parent =>

  def map[B](f : A => B) : Row[B] = new Row[B] {
    def apply(r : db.RowData) : B = f(parent.apply(r))
  }

  def flatMap[B](f : A => Row[B]) : Row[B] = new Row[B] {
    def apply(r : db.RowData) : B = f(parent.apply(r)).apply(r)
  }

  /** Apply two parsers to this same row to produce a new parser with both results. */
  def ~[B](right : Row[B]) : Row[(A,B)] = new Row[(A,B)] {
    def apply(r : db.RowData) : (A,B) = (parent.apply(r), right.apply(r))
  }

  /** Allow any columns parsed by this parser to be null (throwing [[UnexpectedNull]]) and produce None if this is the case. */
  def ? : Row[Option[A]] = new Row[Option[A]] {
    def apply(r : db.RowData) : Option[A] =
      catching(classOf[UnexpectedNull]).opt(parent.apply(r))
  }
}

object Row {
  val identity : Row[db.RowData] = new Row[db.RowData] {
    def apply(r : db.RowData) : db.RowData = r
  }

  def apply[A](f : db.RowData => A) : Row[A] = new Row[A] {
    def apply(r : db.RowData) : A = f(r)
  }

  def apply[A : Type](column : Int) : Row[A] = new Row[A] {
    def apply(r : db.RowData) : A = Type.get[A](r, column)
  }
  def apply[A : Type](column : String) : Row[A] = new Row[A] {
    def apply(r : db.RowData) : A = Type.get[A](r, column)
  }
}

/** A parser for a query result row of a specific size (or a part of the row with said size).
  * @param arity the number of columns consumed by this parser
  * @param get the parser which will be passed only the first (next) arity columns */
class Line[+A](val arity : Int, val get : IndexedSeq[Any] => A) extends Row[A] {
  def apply(r : db.RowData) : A = {
    if (r.length != arity)
      throw new ResultException("got " + r.length + " fields, expecting " + arity)
    get(r)
  }

  override def map[B](f : A => B) = new Line[B](arity, l => f(get(l)))

  /** Join two parsers, effectively splitting each row into two parts to form a tuple. */
  def ~[B](b : Line[B]) : Line[(A,B)] =
    Line.tuple[(A,B)](this, b) { l =>
      (get(l(0)), b.get(l(1)))
    }
  def ~[B,C](b : Line[B], c : Line[C]) : Line[(A,B,C)] =
    Line.tuple[(A,B,C)](this, b, c) { l =>
      (get(l(0)), b.get(l(1)), c.get(l(2)))
    }
  def ~[B,C,D](b : Line[B], c : Line[C], d : Line[D]) : Line[(A,B,C,D)] =
    Line.tuple[(A,B,C,D)](this, b, c, d) { l =>
      (get(l(0)), b.get(l(1)), c.get(l(2)), d.get(l(3)))
    }
  def ~[B,C,D,E](b : Line[B], c : Line[C], d : Line[D], e : Line[E]) : Line[(A,B,C,D,E)] =
    Line.tuple[(A,B,C,D,E)](this, b, c, d, e) { l =>
      (get(l(0)), b.get(l(1)), c.get(l(2)), d.get(l(3)), e.get(l(4)))
    }

  override def ? : Line[Option[A]] = Line[Option[A]](arity) { l =>
    catching(classOf[UnexpectedNull]).opt(get(l))
  }
}

object Line {
  def apply[A](arity : Int)(get : IndexedSeq[Any] => A) =
    new Line[A](arity, get)
  private def tuple[R](lines : Line[_]*)(get : Array[IndexedSeq[Any]] => R) : Line[R] = {
    val arities = lines.map(_.arity).toArray
    Line[R](arities.sum) { l =>
      var n = 0
      get(arities.map { i =>
        l.slice(n, { n += i ; n })
      })
    }
  }
}

abstract sealed class Cols[A] protected (n : Int, f : Iterator[Any] => A) extends Line[A](n, l => f(l.iterator)) {
  // def ~+[C : Type] : Cols[_]
}

object Cols {
  def get[A](i : Iterator[Any])(implicit t : Type[A]) : A = t.get(i.next)

  def apply = new Cols0
  def apply[C1 : Type] = new Cols1[C1]
  def apply[C1 : Type, C2 : Type] = new Cols2[C1, C2]
  def apply[C1 : Type, C2 : Type, C3 : Type] = new Cols3[C1, C2, C3]
  def apply[C1 : Type, C2 : Type, C3 : Type, C4 : Type] = new Cols4[C1, C2, C3, C4]
  def apply[C1 : Type, C2 : Type, C3 : Type, C4 : Type, C5 : Type] = new Cols5[C1, C2, C3, C4, C5]
  def apply[C1 : Type, C2 : Type, C3 : Type, C4 : Type, C5 : Type, C6 : Type] = new Cols6[C1, C2, C3, C4, C5, C6]
  def apply[C1 : Type, C2 : Type, C3 : Type, C4 : Type, C5 : Type, C6 : Type, C7 : Type] = new Cols7[C1, C2, C3, C4, C5, C6, C7]
}

import Cols.get

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
final class Cols0
  extends Cols[Unit](0, i => ()) {
  def map[A](f : => A) : Line[A] = map[A]((_ : Unit) => f)
}
final class Cols1[C1 : Type]
  extends Cols[C1](1, i => get[C1](i)) {
}
final class Cols2[C1 : Type, C2 : Type]
  extends Cols[(C1,C2)](2, i => (get[C1](i), get[C2](i))) {
  def map[A](f : (C1,C2) => A) : Line[A] = map[A](f.tupled)
}
final class Cols3[C1 : Type, C2 : Type, C3 : Type]
  extends Cols[(C1,C2,C3)](3, i => (get[C1](i), get[C2](i), get[C3](i))) {
  def map[A](f : (C1,C2,C3) => A) : Line[A] = map[A](f.tupled)
}
final class Cols4[C1 : Type, C2 : Type, C3 : Type, C4 : Type]
  extends Cols[(C1,C2,C3,C4)](4, i => (get[C1](i), get[C2](i), get[C3](i), get[C4](i))) {
  def map[A](f : (C1,C2,C3,C4) => A) : Line[A] = map[A](f.tupled)
}
final class Cols5[C1 : Type, C2 : Type, C3 : Type, C4 : Type, C5 : Type]
  extends Cols[(C1,C2,C3,C4,C5)](5, i => (get[C1](i), get[C2](i), get[C3](i), get[C4](i), get[C5](i))) {
  def map[A](f : (C1,C2,C3,C4,C5) => A) : Line[A] = map[A](f.tupled)
}
final class Cols6[C1 : Type, C2 : Type, C3 : Type, C4 : Type, C5 : Type, C6 : Type]
  extends Cols[(C1,C2,C3,C4,C5,C6)](6, i => (get[C1](i), get[C2](i), get[C3](i), get[C4](i), get[C5](i), get[C6](i))) {
  def map[A](f : (C1,C2,C3,C4,C5,C6) => A) : Line[A] = map[A](f.tupled)
}
final class Cols7[C1 : Type, C2 : Type, C3 : Type, C4 : Type, C5 : Type, C6 : Type, C7 : Type]
  extends Cols[(C1,C2,C3,C4,C5,C6,C7)](7, i => (get[C1](i), get[C2](i), get[C3](i), get[C4](i), get[C5](i), get[C6](i), get[C7](i))) {
  def map[A](f : (C1,C2,C3,C4,C5,C6,C7) => A) : Line[A] = map[A](f.tupled)
}
