package dbrary

import scala.concurrent.{Future,ExecutionContext}
import scala.util.control.Exception.catching
import com.github.mauricio.async.db
import macros._

class SQLArg[A](val value : A)(implicit val sqlType : SQLType[A]) {
  def put : Any = sqlType.put(value)
  def placeholder = "?::" + sqlType.name
  def escaped : String = sqlType.escaped(value)
}

object SQLArg {
  def apply[A](value : A)(implicit sqlType : SQLType[A]) = new SQLArg[A](value)(sqlType)
}

/** A list of arguments that may be passed to a query. */
class SQLArgs(val args : Seq[SQLArg[_]]) extends Iterable[SQLArg[_]] {
  def iterator = args.iterator
  def ++(other : Iterable[SQLArg[_]]) : SQLArgs = new SQLArgs(args ++ other)
  def :+(other : SQLArg[_]) : SQLArgs = new SQLArgs(args :+ other)
  def +:(other : SQLArg[_]) : SQLArgs = new SQLArgs(other +: args)
  def :+[A : SQLType](other : A) : SQLArgs = new SQLArgs(args :+ SQLArg(other))
  def +:[A : SQLType](other : A) : SQLArgs = new SQLArgs(SQLArg(other) +: args)
  def *(n : Int) : SQLArgs = new SQLArgs(0.until(n).flatMap(_ => args))
}

/** A special case of SQLArgs for running simple queries with no arguments. */
object SQLNoArgs extends SQLArgs(Nil)

/** Generic trait for anything which may accept SQLType args to produce a result. */
protected sealed abstract trait SQLArgsView[+R] extends RepeatedView[SQLType, SQLArg[_], R] {
  protected def arg[A : SQLType](a : A) = SQLArg(a)
  final def apply(a : SQLArgs) : R = result(a.args : _*)
}

object SQLArgs extends SQLArgsView[SQLArgs] {
  protected def result(args : SQLArg[_]*) : SQLArgs = new SQLArgs(args)
  def opt[A : SQLType](o : Option[A]) : SQLArgs = new SQLArgs(o.map(SQLArg(_)).toSeq)
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

  def future(f : Future[db.QueryResult] => Future[db.QueryResult]) : SQLResult =
    new SQLResult(f(result))
  def map[A](f : db.QueryResult => A) : Future[A] = result.map(f)
  def flatMap[A](f : db.QueryResult => Future[A]) : Future[A] = result.flatMap(f)
  def rowsAffected : Future[Long] = map(_.rowsAffected)
  def execute : Future[Boolean] = map(_.rowsAffected > 0)
  def ensure : Future[Unit] = map(r => if (r.rowsAffected > 0) () else fail(r, "no rows affected"))

  def as[A](parse : SQLRow[A]) : SQLRows[A] = new SQLRows[A](result, parse)

  def list[A](parse : SQLRow[A]) : Future[IndexedSeq[A]] = map(rows(_).map(parse(_)))
  def singleOpt[A](parse : SQLRow[A]) : Future[Option[A]] = map(singleOpt(_).map(parse(_)))
  def single[A](parse : SQLRow[A]) : Future[A] = map(r => parse(single(r)))
}

object SQLResult {
  private final val emptyResult = new db.QueryResult(1, "empty") // 1 to emulate "success"
  def empty(implicit context : ExecutionContext) = new SQLResult(async(emptyResult))(context)
}

/** SQLResult with an associated row parser. */
final class SQLRows[+A](result : Future[db.QueryResult], parse : SQLRow[A])(implicit context : ExecutionContext) extends SQLResult(result)(context) {
  override def future(f : Future[db.QueryResult] => Future[db.QueryResult]) : SQLRows[A] =
    new SQLRows[A](f(result), parse)
  def map[B](f : A => B) : SQLRows[B] = new SQLRows[B](result, parse.map[B](f))
  def list : Future[IndexedSeq[A]] = list(parse)
  def singleOpt : Future[Option[A]] = singleOpt(parse)
  def single : Future[A] = single(parse)
}

/** A generic row parser that can transform RowData query results. */
trait SQLRow[+A] extends (db.RowData => A) {
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
class SQLLine[+A](val arity : Int, val get : IndexedSeq[Any] => A) extends SQLRow[A] {
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
  def ~[B,C](b : SQLLine[B], c : SQLLine[C]) : SQLLine[(A,B,C)] =
    new SQLLine[(A,B,C)](arity + b.arity + c.arity, { l =>
      val (la, lbc) = l.splitAt(arity) 
      val (lb, lc) = lbc.splitAt(b.arity)
      (get(la), b.get(lb), c.get(lc))
    })
  def ~[B,C,D](b : SQLLine[B], c : SQLLine[C], d : SQLLine[D]) : SQLLine[(A,B,C,D)] =
    new SQLLine[(A,B,C,D)](arity + b.arity + c.arity + d.arity, { l =>
      val (la, lbcd) = l.splitAt(arity) 
      val (lb, lcd) = lbcd.splitAt(b.arity)
      val (lc, ld) = lbcd.splitAt(c.arity)
      (get(la), b.get(lb), c.get(lc), d.get(ld))
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
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType, C6 : SQLType] = new SQLCols6[C1, C2, C3, C4, C5, C6]
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType, C6 : SQLType, C7 : SQLType] = new SQLCols7[C1, C2, C3, C4, C5, C6, C7]
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
final class SQLCols6[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType, C6 : SQLType]
  extends SQLCols[(C1,C2,C3,C4,C5,C6)](6, i => (get[C1](i), get[C2](i), get[C3](i), get[C4](i), get[C5](i), get[C6](i))) {
  def map[A](f : (C1,C2,C3,C4,C5,C6) => A) : SQLLine[A] = map[A](f.tupled)
}
final class SQLCols7[C1 : SQLType, C2 : SQLType, C3 : SQLType, C4 : SQLType, C5 : SQLType, C6 : SQLType, C7 : SQLType]
  extends SQLCols[(C1,C2,C3,C4,C5,C6,C7)](7, i => (get[C1](i), get[C2](i), get[C3](i), get[C4](i), get[C5](i), get[C6](i), get[C7](i))) {
  def map[A](f : (C1,C2,C3,C4,C5,C6,C7) => A) : SQLLine[A] = map[A](f.tupled)
}

/** A generic class representing a query which may (or may not) be applied to arguments, and produces a particular result type.
  * @param query SQL statement */
protected sealed abstract class SQLBuilder[+A] protected (val query : String)(implicit dbconn : db.Connection, context : ExecutionContext) extends SQLArgsView[A] {
  protected def send(args : Seq[SQLArg[_]], prepared : Boolean) : Future[db.QueryResult] = {
    val r = if (args.isEmpty)
      dbconn.sendQuery(query)
    else if (prepared)
      dbconn.sendPreparedStatement(query, args.map(_.put))
    else
      dbconn.sendQuery(SQL.substituteArgs(query, args))
    if (SQL.logger.isTraceEnabled) {
      val t0 = System.nanoTime
      r.onComplete { r =>
        SQL.logger.trace(((System.nanoTime - t0) / 1e9).formatted("%8.5f: ") + (if (args.nonEmpty) if (prepared) "{" + args.length + "}" else "[" + args.length + "]" else "") + query)
      }
    }
    r
  }
}

/** A simple query which may be applied to arguments, producing a SQLResult. */
final class SQL private (query : String, prepared : Boolean = true)(implicit dbconn : db.Connection, context : ExecutionContext) extends SQLBuilder[SQLResult](query)(dbconn, context) {
  final protected def result(args : SQLArg[_]*) : SQLResult = new SQLResult(send(args, prepared))
  final protected def as[A](parse : SQLRow[A]) : SQLToRows[A] = new SQLToRows(query, parse, prepared)(dbconn, context)
  def execute() : Future[Unit] = send(Nil, prepared).map(_ => ())
  def immediately = if (prepared) new SQL(query, false)(dbconn, context) else this
  def prepare = if (prepared) this else new SQL(query, true)(dbconn, context)
}
object SQL {
  def apply(q : String*)(implicit dbc : db.Connection, context : ExecutionContext) : SQL =
    new SQL(unwords(q : _*))(dbc, context)
  private[dbrary] def quoted(s : String) =
    "'" + s.replaceAllLiterally("'", "''") + "'";
  private[dbrary] val logger : play.api.Logger = play.api.Logger("sql")
  private[dbrary] def substituteArgs(query : String, args : Seq[SQLArg[_]]) : String = {
    /* based on db.postgresql.PreparedStatementHolder */
    val result = new StringBuilder(query.length + 16*args.length)
    var offset = 0
    @scala.annotation.tailrec def next() : Boolean = {
      val i = query.indexOf('?', offset)
      if (i == -1) {
        result ++= query.substring(offset)
        return false
      }
      result ++= query.substring(offset, i)
      offset = i + 1
      if (offset < query.length && query(offset) == '?') {
        result += '?'
        offset += 1
        next()
      } else
        true
    }
    for (arg <- args) {
      if (!next())
        throw new db.exceptions.InsufficientParametersException(0 /* whatever */, args)
      result ++= arg.escaped
    }
    if (next())
      throw new db.exceptions.InsufficientParametersException(args.length+1 /* whatever */, args)
    result.toString
  }
}

/** A query which may be applied to arguments, producing rows to be parsed to a particular type.
  * @param parse the parser to use on result rows */
final class SQLToRows[+A](query : String, parse : SQLRow[A], prepared : Boolean, val preargs : Seq[SQLArg[_]] = Nil)(implicit dbconn : db.Connection, context : ExecutionContext) extends SQLBuilder[SQLRows[A]](query)(dbconn, context) {
  final protected def result(args : SQLArg[_]*) : SQLRows[A] = new SQLRows(send(preargs ++ args, prepared), parse)
  def immediately = if (prepared) new SQLToRows[A](query, parse, false, preargs)(dbconn, context) else this
  def prepare = if (prepared) this else new SQLToRows[A](query, parse, true, preargs)(dbconn, context)
}
