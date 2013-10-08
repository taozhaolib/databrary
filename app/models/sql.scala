package models

import anorm._
import site._

/** Parameters (names and values) that may be passed to SQL queries. */
private[models] final class SQLArgs private (private val args : Seq[SQLArgs.Arg]) extends scala.collection.SeqProxy[(String,ParameterValue[_])] {
  def self = args.map { case (p,v) => (p.name,v) }
  def ++(other : SQLArgs) : SQLArgs = new SQLArgs(args ++ other.args)
  // def :+(other : SQLArgs.Arg) : SQLArgs = new SQLArgs(args :+ other)
  // def +:(other : SQLArgs.Arg) : SQLArgs = new SQLArgs(other +: args)
  private lazy val names = args.map(_._1.name)

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
    names.map(n => n + " = {" + n + "}").mkString(sep)
  def where = set(" AND ")
}
private[models] object SQLArgs {
  /** A single SQL placeholder parameter and its value. */
  type Arg = (Symbol, ParameterValue[_])
  def apply(args : Arg*) = new SQLArgs(args)
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

