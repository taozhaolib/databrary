package models

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db
import macros._
import dbrary._
import site._

class SQLTerm[A](val name : String, value : A)(implicit sqlType : SQLType[A]) extends SQLArg[A](value)(sqlType) {
}
object SQLTerm {
  def apply[A](name : String, arg : SQLArg[A]) = new SQLTerm[A](name, arg.value)(arg.sqlType)
  def apply[A](name : String, value : A)(implicit sqlType : SQLType[A]) = new SQLTerm[A](name, value)(sqlType)
  import scala.language.implicitConversions
  implicit def ofTuple[A : SQLType](x : (Symbol, A)) : SQLTerm[A] = SQLTerm[A](x._1.name, x._2)
}

/** Parameters (names and values) that may be passed to SQL queries. */
final class SQLTerms private (private val terms : Seq[SQLTerm[_]]) extends SQLArgs(terms) {
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
object SQLTerms {
  def apply(terms : SQLTerm[_]*) = new SQLTerms(terms)
  def flatten(terms : Option[SQLTerm[_]]*) = new SQLTerms(terms.flatten)
}

object SQLDuplicateKeyException {
  def unapply(e : db.postgresql.exceptions.GenericDatabaseException) : Boolean =
    e.errorMessage.message.startsWith("duplicate key value violates unique constraint ")
}

object DBUtil {
  def selectOrInsert[A](select : (Site.DB, ExecutionContext) => Future[Option[A]])(insert : (Site.DB, ExecutionContext) => Future[A])(implicit dbc : Site.DB, exc : ExecutionContext) : Future[A] = {
    val sp = "pre_insert"
    /*@scala.annotation.tailrec*/ def loop(dbc : Site.DB) : Future[A] = select(dbc, exc).flatMap {
      case None =>
	SQL("SAVEPOINT", sp)(dbc, exc).execute.flatMap { _ =>
	insert(dbc, exc).recoverWith {
        case SQLDuplicateKeyException() =>
	  SQL("ROLLBACK TO SAVEPOINT", sp)(dbc, exc).execute.flatMap { _ =>
	    loop(dbc)
	  }
	}
	}
      case Some(r) => async(r)
    }
    dbc.inTransaction(loop)
  }

  def updateOrInsert(update : (Site.DB, ExecutionContext) => SQLResult)(insert : (Site.DB, ExecutionContext) => SQLResult)(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult = {
    val sp = "pre_insert"
    /*@scala.annotation.tailrec*/ def loop(dbc : Site.DB) : Future[db.QueryResult] = update(dbc, exc).result.flatMap { r =>
      if (r.rowsAffected == 0)
	SQL("SAVEPOINT", sp)(dbc, exc).execute.flatMap { _ =>
        insert(dbc, exc).result.recoverWith {
          case SQLDuplicateKeyException() =>
	    SQL("ROLLBACK TO SAVEPOINT", sp)(dbc, exc).execute.flatMap { _ =>
	      loop(dbc)
	    }
        }
	}
      else
        async(r)
    }
    new SQLResult(dbc.inTransaction(loop))
  }
}
