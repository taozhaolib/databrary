package dbrary

import scala.concurrent.{Future,ExecutionContext}
import scala.util.control.Exception.catching
import com.github.mauricio.async.db
import macros._

/** Arguments that may be passed to a query. */
trait SQLArgs {
  def args : Seq[Any]
}

/** A special case of SQLArgs for running simple queries with no arguments. */
object SQLNoArgs extends SQLArgs {
  val args = Nil
}

/** A list of marshalled arguments for passing to a query. */
final class SQLArgseq(val args : /*=>*/ Seq[Any]) extends SQLArgs {
  import SQLType.put

  def ++(other : SQLArgseq) : SQLArgseq = new SQLArgseq(args ++ other.args)
  def :+[A : SQLType](other : A) : SQLArgseq = new SQLArgseq(args :+ put[A](other))
  def +:[A : SQLType](other : A) : SQLArgseq = new SQLArgseq(put[A](other) +: args)
  def *(n : Int) : SQLArgseq = new SQLArgseq(0.until(n).flatMap(_ => args))
}

/** Generic trait for anything which may accept SQLType args to produce a result. */
protected sealed abstract trait SQLArgsView[R] extends RepeatedView[SQLType, Any, R] {
  protected def arg[A : SQLType](a : A) = SQLType.put[A](a)
  final def apply(a : SQLArgs) : R = result(a.args : _*)
}

object SQLArgs extends SQLArgsView[SQLArgseq] {
  protected def result(args : Any*) : SQLArgseq = new SQLArgseq(args)
}

/** Exceptions that may occur during parsing query results, usually indicating type or arity mismatches. */
class SQLResultException(message : String) extends db.exceptions.DatabaseException(message)

/** A simple wrapper for Future[QueryResult] representing the result of a query. */
class SQLResult(val result : Future[db.QueryResult])(implicit context : ExecutionContext) {
  private[this] def fail(res : db.QueryResult, msg : String) = throw new SQLResultException(res.statusMessage + ": " + msg)
  private[this] def rows(res : db.QueryResult) = res.rows.getOrElse(fail(res, "no results"))
  private[this] def singleOpt(res : db.QueryResult) = rows(res) match {
    case Seq() => None
    case Seq(r) => Some(r)
    case l => fail(res, "got " + l.length + " rows, expected 1")
  }
  private[this] def single(res : db.QueryResult) = singleOpt(res).getOrElse(fail(res, "got no rows, expected 1"))

  def map[A](f : db.QueryResult => A) : Future[A] = result.map(f)
  def flatMap[A](f : db.QueryResult => Future[A]) : Future[A] = result.flatMap(f)
  def rowsAffected : Future[Long] = map(_.rowsAffected)
  def execute : Future[Boolean] = map(_.rowsAffected > 0)
  /** A dummy marker to show where we execute independent queries. */
  @deprecated("this usage will not catch errors", "0") def run() : Unit = ()

  def as[A](parse : SQLRow[A]) : SQLRows[A] = new SQLRows[A](result, parse)

  def list[A](parse : SQLRow[A]) : Future[IndexedSeq[A]] = map(rows(_).map(parse(_)))
  def singleOpt[A](parse : SQLRow[A]) : Future[Option[A]] = map(singleOpt(_).map(parse(_)))
  def single[A](parse : SQLRow[A]) : Future[A] = map(r => parse(single(r)))
}

/** SQLResult with an associated row parser. */
final class SQLRows[A](result : Future[db.QueryResult], parse : SQLRow[A])(implicit context : ExecutionContext) extends SQLResult(result)(context) {
  def map[B](f : A => B) : SQLRows[B] = new SQLRows[B](result, parse.map[B](f))
  def list : Future[IndexedSeq[A]] = list(parse)
  def singleOpt : Future[Option[A]] = singleOpt(parse)
  def single : Future[A] = single(parse)
}

/** A generic row parser that can transform RowData query results. */
trait SQLRow[A] extends (db.RowData => A) {
  parent =>

  def map[B](f : A => B) : SQLRow[B] = new SQLRow[B] {
    def apply(r : db.RowData) : B = f(parent.apply(r))
  }

  def flatMap[B](f : A => SQLRow[B]) : SQLRow[B] = new SQLRow[B] {
    def apply(r : db.RowData) : B = f(parent.apply(r)).apply(r)
  }

  /** Apply two parsers to this same row to produce a new parser with both results. */
  def ~[B](right : SQLRow[B]) : SQLRow[(A,B)] = new SQLRow[(A,B)] {
    def apply(r : db.RowData) : (A,B) = (parent.apply(r), right.apply(r))
  }

  /** Allow any columns parsed by this parser to be null (throwing [[SQLUnexpectedNull]]) and produce None if this is the case. */
  def ? : SQLRow[Option[A]] = new SQLRow[Option[A]] {
    def apply(r : db.RowData) : Option[A] =
      catching(classOf[SQLUnexpectedNull]).opt(parent.apply(r))
  }
}

object SQLRow {
  val identity : SQLRow[db.RowData] = new SQLRow[db.RowData] {
    def apply(r : db.RowData) : db.RowData = r
  }

  def apply[A](f : db.RowData => A) : SQLRow[A] = new SQLRow[A] {
    def apply(r : db.RowData) : A = f(r)
  }

  def apply[A : SQLType](column : Int) : SQLRow[A] = new SQLRow[A] {
    def apply(r : db.RowData) : A = SQLType.get[A](r, column)
  }
  def apply[A : SQLType](column : String) : SQLRow[A] = new SQLRow[A] {
    def apply(r : db.RowData) : A = SQLType.get[A](r, column)
  }
}

/** A parser for a query result row of a specific size (or a part of the row with said size).
  * @param arity the number of columns consumed by this parser
  * @param get the parser which will be passed only the first (next) arity columns */
class SQLLine[A](val arity : Int, val get : IndexedSeq[Any] => A) extends SQLRow[A] {
  def apply(r : db.RowData) : A = {
    if (r.length != arity)
      throw new SQLResultException("got " + r.length + " fields, expecting " + arity)
    get(r)
  }

  override def map[B](f : A => B) = new SQLLine[B](arity, l => f(get(l)))

  /** Join two parsers, effectively splitting each row into two parts to form a tuple. */
  def ~[B](b : SQLLine[B]) : SQLLine[(A,B)] =
    new SQLLine[(A,B)](arity + b.arity, { l =>
      val (la, lb) = l.splitAt(arity) 
      (get(la), b.get(lb))
    })

  override def ? : SQLLine[Option[A]] = new SQLLine[Option[A]](arity, l =>
    catching(classOf[SQLUnexpectedNull]).opt(get(l)))
}

abstract sealed class SQLCols[A] protected (n : Int, f : Iterator[Any] => A) extends SQLLine[A](n, l => f(l.iterator)) {
  // def ~+[C : SQLType] : SQLCols[_]
}

object SQLCols {
  def get[A](i : Iterator[Any])(implicit t : SQLType[A]) : A = t.get(i.next)

  def apply = new SQLCols0
  def apply[C1 : SQLType] = new SQLCols1[C1]
  def apply[C1 : SQLType, C2 : SQLType] = new SQLCols2[C1, C2]
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType] = new SQLCols3[C1, C2, C3]
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType] = new SQLCols4[C1, C2, C3, C4]
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType] = new SQLCols5[C1, C2, C3, C4, C5]
}

import SQLCols.get

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
final class SQLCols0
  extends SQLCols[Unit](0, i => ()) {
  def map[A](f : => A) : SQLLine[A] = map[A]((_ : Unit) => f)
}
final class SQLCols1[C1 : SQLType]
  extends SQLCols[C1](1, i => get[C1](i)) {
}
final class SQLCols2[C1 : SQLType, C2 : SQLType]
  extends SQLCols[(C1,C2)](2, i => (get[C1](i), get[C2](i))) {
  def map[A](f : (C1,C2) => A) : SQLLine[A] = map[A](f.tupled)
}
final class SQLCols3[C1 : SQLType, C2 : SQLType, C3 : SQLType]
  extends SQLCols[(C1,C2,C3)](3, i => (get[C1](i), get[C2](i), get[C3](i))) {
  def map[A](f : (C1,C2,C3) => A) : SQLLine[A] = map[A](f.tupled)
}
final class SQLCols4[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType]
  extends SQLCols[(C1,C2,C3,C4)](4, i => (get[C1](i), get[C2](i), get[C3](i), get[C4](i))) {
  def map[A](f : (C1,C2,C3,C4) => A) : SQLLine[A] = map[A](f.tupled)
}
final class SQLCols5[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType]
  extends SQLCols[(C1,C2,C3,C4,C5)](5, i => (get[C1](i), get[C2](i), get[C3](i), get[C4](i), get[C5](i))) {
  def map[A](f : (C1,C2,C3,C4,C5) => A) : SQLLine[A] = map[A](f.tupled)
}

/** A generic class representing a query which may (or may not) be applied to arguments, and produces a particular result type.
  * @param query SQL statement */
protected sealed abstract class SQLBuilder[A] protected (val query : String)(implicit dbconn : db.Connection, context : ExecutionContext) extends SQLArgsView[A] {
  protected def send(args : Seq[Any]) : Future[db.QueryResult] = {
    if (args.isEmpty)
      dbconn.sendQuery(query)
    else
      dbconn.sendPreparedStatement(query, args)
  }
}

/** A simple query which may be applied to arguments, producing a SQLResult. */
final case class SQL(override val query : String)(implicit dbconn : db.Connection, context : ExecutionContext) extends SQLBuilder[SQLResult](query)(dbconn, context) {
  final protected def result(args : Any*) : SQLResult = new SQLResult(send(args))
  final protected def as[A](parse : SQLRow[A]) : SQLToRows[A] = new SQLToRows(query, parse)(dbconn, context)
}

/** A query which may be applied to arguments, producing rows to be parsed to a particular type.
  * @param parse the parser to use on result rows */
final case class SQLToRows[A](override val query : String, parse : SQLRow[A])(implicit dbconn : db.Connection, context : ExecutionContext) extends SQLBuilder[SQLRows[A]](query)(dbconn, context) {
  final protected def result(args : Any*) : SQLRows[A] = new SQLRows(send(args), parse)
}
