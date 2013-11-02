package models

import site._

case class SQLTerm[A : SQLType](name : String, value : A) {
  def put : Any = SQLType.put[A](value)
}

/** Parameters (names and values) that may be passed to SQL queries. */
private[models] final class SQLTerms private (private val terms : Seq[SQLTerm[_]]) extends SQLArgs {
  def ++(other : SQLTerms) : SQLTerms = new SQLTerms(terms ++ other.terms)
  def :+(other : SQLTerm[_]) : SQLTerms = new SQLTerms(terms :+ other)
  def +:(other : SQLTerm[_]) : SQLTerms = new SQLTerms(other +: terms)
  def args : Seq[Any] = terms.map(_.put)
  private lazy val names = terms.map(_.name)

  /** Terms appropriate for INSERT INTO statements.
    * @returns `(arg, ...) VALUES ({arg}, ...)`
    */
  def insert =
    names.mkString("(", ", ", ")") + " VALUES " + names.mkString("({", "}, {", "})")
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

object DBUtil {
  def selectOrInsert[A](select : => Option[A])(insert : => A)(implicit db : Site.DB) : A = {
    @scala.annotation.tailrec def loop : A = select orElse {
      val sp = db.setSavepoint
      try {
        Some(insert)
      } catch {
        case e : java.sql.SQLException if e.getMessage.startsWith("ERROR: duplicate key value violates unique constraint ") =>
          db.rollback(sp)
          None
      } finally {
        db.releaseSavepoint(sp)
      }
    } match {
      case Some(r) => r
      case None => loop
    }
    loop
  }

  def updateOrInsert(update : Sql)(insert : Sql)(implicit db : Site.DB) : Unit = {
    @scala.annotation.tailrec def loop() : Unit = {
      if (update.executeUpdate() == 0 && !{
        val sp = db.setSavepoint
        try {
          insert.execute()
          true
        } catch {
          case e : java.sql.SQLException if e.getMessage.startsWith("ERROR: duplicate key value violates unique constraint ") =>
            db.rollback(sp)
            false
        } finally {
          db.releaseSavepoint(sp)
        }
      }) loop()
    }
    loop()
  }
}

