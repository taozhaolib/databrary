package models

import dbrary._
import site._

case class SQLTerm[A : SQLType](name : String, value : A) {
  def put : Any = SQLType.put[A](value)
}
object SQLTerm {
  import scala.language.implicitConversions
  implicit def apply[A : SQLType](x : (Symbol, A)) : SQLTerm[A] = SQLTerm[A](x._1.name, x._2)
}

/** Parameters (names and values) that may be passed to SQL queries. */
private[models] final class SQLTerms private (private val terms : Seq[SQLTerm[_]]) extends SQLArgs {
  def ++(other : SQLTerms) : SQLTerms = new SQLTerms(terms ++ other.terms)
  def :+(other : SQLTerm[_]) : SQLTerms = new SQLTerms(terms :+ other)
  def +:(other : SQLTerm[_]) : SQLTerms = new SQLTerms(other +: terms)
  def args : Seq[Any] = terms.map(_.put)
  private lazy val names = terms.map(_.name)
  def placeholders : String = args.length match {
    case 0 => ""
    case n => "?" + (", ?" * (n-1))
  }

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
    names.map(_ + " = ?").mkString(sep)
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
  def selectOrInsert[A](select : Site.DB => Future[Option[A]])(insert : Site.DB => Future[A])(implicit dbc : Site.DB) : Future[A] = {
    @scala.annotation.tailrec def loop() : Future[A] = select(dbc).flatMap {
      case None => insert(dbc).recoverWith {
        case SQLDuplicateKeyException => loop
      }
      case Some(r) => Future.successful(r)
    }
    loop()
  }

  def updateOrInsert(update : SQLResult)(insert : SQLResult)(implicit dbc : Site.DB) : SQLResult = {
    @scala.annotation.tailrec def loop() : SQLResult = update(dbc).flatMap { r =>
      if (r.rowsAffected == 0)
        insert(dbc).recoverWith {
          case SQLDuplicateKeyException => loop
        }
      else
        Future.successful(r)
    }
    loop()
  }
}
