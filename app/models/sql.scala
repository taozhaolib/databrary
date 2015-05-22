package models

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db
import macros._
import dbrary._
import dbrary.SQL._
import site._

class SQLTerm[A](val name : String, value : A)(implicit sqlType : SQL.Type[A]) extends SQL.Arg[A](value)(sqlType) {
  def eq : String = "="
  def withEq(eq : String) = new SQLTermEq[A](name, eq, value)(sqlType)
}
class SQLTermEq[A](name : String, override val eq : String, value : A)(implicit sqlType : SQL.Type[A]) extends SQLTerm[A](name, value)(sqlType)
object SQLTerm {
  def apply[A](name : String, arg : SQL.Arg[A]) = new SQLTerm[A](name, arg.value)(arg.sqlType)
  def apply[A](name : String, value : A)(implicit sqlType : SQL.Type[A]) = new SQLTerm[A](name, value)(sqlType)
  def eq[A](name : String, eq : String, value : A)(implicit sqlType : SQL.Type[A]) = new SQLTermEq[A](name, eq, value)(sqlType)
  import scala.language.implicitConversions
  implicit def ofTuple[A : SQL.Type](x : (Symbol, A)) : SQLTerm[A] = SQLTerm[A](x._1.name, x._2)
}

/** Parameters (names and values) that may be passed to SQL queries. */
final class SQLTerms private (private val terms : Seq[SQLTerm[_]]) extends SQL.Args(terms) {
  def ++(other : SQLTerms) : SQLTerms = new SQLTerms(terms ++ other.terms)
  def :+(other : SQLTerm[_]) : SQLTerms = new SQLTerms(terms :+ other)
  def +:(other : SQLTerm[_]) : SQLTerms = new SQLTerms(other +: terms)
  def :+[A : SQL.Type](other : (Symbol, A)) : SQLTerms = new SQLTerms(terms :+ SQLTerm.ofTuple(other))
  def +:[A : SQL.Type](other : (Symbol, A)) : SQLTerms = new SQLTerms(SQLTerm.ofTuple(other) +: terms)
  def names = terms.map(_.name).mkString("(", ",", ")")
  def values = ("VALUES (" +: join(",")) + ")"

  /** Terms appropriate for INSERT INTO statements.
    * @return `(arg, ...) VALUES (?, ...)`
    */
  def insert = names +: values
  /** Terms appropriate for UPDATE or WHERE statements.
    * @param sep separator string, ", " for UPDATE (default), " AND " for WHERE
    * @return `arg = ? sep ...`
    */
  def set(sep : String = ",") =
    Statement.join(sep, terms.map(t => (t.name + t.eq) +: t) : _*)
  def where = set(" AND ")
  /** Constant table.
    * @return `(VALUES (?, ...)) AS table (arg, ...)`
    */
  def fixed(implicit table : FromTable) : Selector[Unit] =
    Columns(table).fromQuery(values).mapFrom(_ + names)
}
object SQLTerms {
  def apply(terms : SQLTerm[_]*) = new SQLTerms(terms)
  def flatten(terms : Option[SQLTerm[_]]*) = new SQLTerms(terms.flatten)
}

object SQLException {
  def unapply(e : db.postgresql.exceptions.GenericDatabaseException) : Option[String] =
    return Some(e.errorMessage.message)
}

object SQLDuplicateKeyException {
  def unapply(e : db.postgresql.exceptions.GenericDatabaseException) : Boolean = {
    val msg = e.errorMessage.message
    msg.startsWith("duplicate key value violates unique constraint ") ||
    msg.startsWith("conflicting key value violates exclusion constraint ")
  }
}

object DBUtil {
  def selectOrInsert[A](select : (Site.DB, ExecutionContext) => Future[Option[A]])(insert : (Site.DB, ExecutionContext) => Future[A])(implicit dbc : Site.DB, exc : ExecutionContext) : Future[A] = {
    val sp = "pre_insert"
    /*@scala.annotation.tailrec*/ def loop(dbc : Site.DB) : Future[A] = select(dbc, exc).flatMap {
      case None =>
        LiteralStatement("SAVEPOINT " + sp).run(dbc, exc).execute.flatMap { _ =>
        insert(dbc, exc).recoverWith {
        case SQLDuplicateKeyException() =>
          LiteralStatement("ROLLBACK TO SAVEPOINT " + sp).run(dbc, exc).execute.flatMap { _ =>
            loop(dbc)
          }
        }
        }
      case Some(r) => async(r)
    }
    dbc.inTransaction(loop)
  }

  def updateOrInsert(update : (Site.DB, ExecutionContext) => SQL.Result)(insert : (Site.DB, ExecutionContext) => SQL.Result)(implicit dbc : Site.DB, exc : ExecutionContext) : SQL.Result = {
    val sp = "pre_insert"
    /*@scala.annotation.tailrec*/ def loop(dbc : Site.DB) : Future[db.QueryResult] = update(dbc, exc).result.flatMap { r =>
      if (r.rowsAffected == 0)
        LiteralStatement("SAVEPOINT " + sp).run(dbc, exc).execute.flatMap { _ =>
        insert(dbc, exc).result.recoverWith {
          case SQLDuplicateKeyException() =>
            LiteralStatement("ROLLBACK TO SAVEPOINT " + sp).run(dbc, exc).execute.flatMap { _ =>
              loop(dbc)
            }
        }
        }
      else
        async(r)
    }
    new SQL.Result(dbc.inTransaction(loop))
  }
}
