package models

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db
import macros._
import dbrary._
import site._

class SQLTerm[A](val name : String, value : A)(implicit sqlType : SQLType[A]) extends SQLArg[A](value)(sqlType) {
}
object SQLTerm {
  def apply[A](name : String, value : A)(implicit sqlType : SQLType[A]) = new SQLTerm[A](name, value)(sqlType)
  import scala.language.implicitConversions
  implicit def ofTuple[A : SQLType](x : (Symbol, A)) : SQLTerm[A] = SQLTerm[A](x._1.name, x._2)
}

/** Parameters (names and values) that may be passed to SQL queries. */
private[models] final class SQLTerms private (private val terms : Seq[SQLTerm[_]]) extends SQLArgs(terms) {
  def ++(other : SQLTerms) : SQLTerms = new SQLTerms(terms ++ other.terms)
  def :+(other : SQLTerm[_]) : SQLTerms = new SQLTerms(terms :+ other)
  def +:(other : SQLTerm[_]) : SQLTerms = new SQLTerms(other +: terms)
  def :+[A : SQLType](other : (Symbol, A)) : SQLTerms = new SQLTerms(terms :+ SQLTerm.ofTuple(other))
  def +:[A : SQLType](other : (Symbol, A)) : SQLTerms = new SQLTerms(SQLTerm.ofTuple(other) +: terms)
  def names = terms.map(_.name).mkString("(", ", ", ")")
  def placeholders : String = terms.map(_.placeholder).mkString(", ")

  /** Terms appropriate for INSERT INTO statements.
    * @returns `(arg, ...) VALUES (?, ...)`
    */
  def insert =
    names + " VALUES (" + placeholders + ")"
  /** Terms appropriate for UPDATE or WHERE statements.
    * @param sep separator string, ", " for UPDATE (default), " AND " for WHERE
    * @returns `arg = ? sep ...`
    */
  def set(sep : String = ", ") =
    terms.map(t => t.name + " = " + t.placeholder).mkString(sep)
  def where = set(" AND ")
  /** Constant table.
    * @returns `(VALUES (?, ...)) AS table (arg, ...)`
    */
  def values(implicit table : FromTable) : Selector[Unit] =
    Columns(FromTable("(VALUES (" + placeholders + ")) AS " + table + " " + names))
    .pushArgs(this)
}
private[models] object SQLTerms {
  def apply(terms : SQLTerm[_]*) = new SQLTerms(terms)
  def flatten(terms : Option[SQLTerm[_]]*) = new SQLTerms(terms.flatten)
}

object SQLDuplicateKeyException {
  def unapply(e : db.postgresql.exceptions.GenericDatabaseException) : Boolean =
    e.errorMessage.message.startsWith("duplicate key value violates unique constraint ")
}

object DBUtil {
  /* TODO: wrap these in transactions once available */
  def selectOrInsert[A](select : (Site.DB, ExecutionContext) => Future[Option[A]])(insert : (Site.DB, ExecutionContext) => Future[A])(implicit dbc : Site.DB, exc : ExecutionContext) : Future[A] = {
    /*@scala.annotation.tailrec*/ def loop : Future[A] = select(dbc, exc).flatMap {
      case None => insert(dbc, exc).recoverWith {
        case SQLDuplicateKeyException() => loop
      }
      case Some(r) => Async(r)
    }
    loop
  }

  def updateOrInsert(update : (Site.DB, ExecutionContext) => SQLResult)(insert : (Site.DB, ExecutionContext) => SQLResult)(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult = {
    /*@scala.annotation.tailrec*/ def loop : Future[db.QueryResult] = update(dbc, exc).result.flatMap { r =>
      if (r.rowsAffected == 0)
        insert(dbc, exc).result.recoverWith {
          case SQLDuplicateKeyException() => loop
        }
      else
        Async(r)
    }
    new SQLResult(loop)
  }
}

private[models] sealed abstract class ObjectSelector[A] {
  def selector : Selector[A]
  def obj : Option[A]
}

private[models] object ObjectSelector {
  private final class Sel[A](val selector : Selector[A]) extends ObjectSelector[A] {
    def obj = None
  }

  private final class Obj[A](o : A, table : String, cols : SQLTerms) extends ObjectSelector[A] {
    def selector =
      Columns(FromTable("(VALUES (" + cols.placeholders + ")) AS " + table + " " + cols.names))
      .pushArgs(cols)
      .map(_ => o)
    def obj = Some(o)
  }

  def apply[A](obj : A, cols : SQLTerms)(implicit table : FromTable) : ObjectSelector[A] =
    new Obj[A](obj, table.table, cols)

  import scala.language.implicitConversions
  implicit def ofSelector[A](selector : Selector[A]) : ObjectSelector[A] =
    new Sel[A](selector)
  implicit def toSelector[A](os : ObjectSelector[A]) : Selector[A] =
    os.selector
}
