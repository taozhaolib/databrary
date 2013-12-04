package models

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db
import macros._
import dbrary._
import site._

case class SQLTerm[A](name : String, value : A)(implicit val sqlType : SQLType[A]) {
  def put : Any = SQLType.put[A](value)
}
object SQLTerm {
  import scala.language.implicitConversions
  implicit def ofTuple[A : SQLType](x : (Symbol, A)) : SQLTerm[A] = SQLTerm[A](x._1.name, x._2)
}

/** Parameters (names and values) that may be passed to SQL queries. */
private[models] final class SQLTerms private (private val terms : Seq[SQLTerm[_]]) extends SQLArgs {
  def ++(other : SQLTerms) : SQLTerms = new SQLTerms(terms ++ other.terms)
  def :+(other : SQLTerm[_]) : SQLTerms = new SQLTerms(terms :+ other)
  def +:(other : SQLTerm[_]) : SQLTerms = new SQLTerms(other +: terms)
  def args : Seq[Any] = terms.map(_.put)
  private lazy val names = terms.map(_.name)
  def placeholders : String = terms.map("?::" + _.sqlType.name).mkString(", ")

  /** Terms appropriate for INSERT INTO statements.
    * @returns `(arg, ...) VALUES ({arg}, ...)`
    */
  def insert =
    names.mkString("(", ", ", ")") + " VALUES (" + placeholders + ")"
  /** Terms appropriate for UPDATE or WHERE statements.
    * @param sep separator string, ", " for UPDATE (default), " AND " for WHERE
    * @returns `arg = {arg} sep ...`
    */
  def set(sep : String = ", ") =
    terms.map(t => t.name + " = ?::" + t.sqlType.name).mkString(sep)
  def where = set(" AND ")
}
private[models] object SQLTerms {
  def apply(terms : SQLTerm[_]*) = new SQLTerms(terms)
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
