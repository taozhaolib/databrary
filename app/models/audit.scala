package models

import scala.concurrent.{Future,ExecutionContext}
import macros._
import dbrary._
import dbrary.SQL._
import site.Site

abstract class AuditBase(when : Timestamp, who : Party.Id, ip : Inet, action : Audit.Action.Value) extends TableRow {
  private[models] def sqlKey = SQLTerms()

  /** Look up the party who generated this event, if still valid. */
  def party(implicit site : Site) = Party.get(who)(site)
}

/** Represents an event row in an audit table of a particular type.  Currently unused as there is intentionally no read/modify access to audit tables.
  * @constructor Create a new unpersisted audit record. There is no reason to do this.
  * @tparam T the type of data attached to this record. Usually this is a TableRow instance for a corresponding audit_t table.
  * @param when the time of this event
  * @param who the user who generated this event. May no longer be a valid party.
  * @param ip the ip of the client machine that generated this event
  * @param action the type of event
  * @param row the remaining data columns
  */
final case class Audit[T](when : Timestamp, who : Party.Id, ip : Inet, action : Audit.Action.Value, row : T) extends AuditBase(when, who, ip, action) {
  def withRow[A](row : A) = copy[A](row = row)
}

/** Helper for audit tables. */
object Audit extends Table[Audit[_]]("audit.audit") {
  /** The possible events or actions on the site that can be put into audit tables. */
  object Action extends SQL.Enum("audit_action") {
    val attempt, open, close, add, change, remove, superuser = Value
  }

  private[models] def row[T](tableName : String = "audit", row : T = ()) = {
    implicit val fromTable : FromTable = FromTable(tableName)
    Columns(
      SelectColumn[Timestamp]("audit_time"),
      SelectColumn[Party.Id]("audit_user"),
      SelectColumn[Inet]("audit_ip"),
      SelectColumn[Action.Value]("audit_action"))
      .map { (when, who, ip, action) =>
        new Audit[T](when, who, ip, action, row)
      }
  }
  private[models] val columns = row[Unit]()

  private[this] def acmd(action : Action.Value) = action match {
    case Action.add => "INSERT INTO"
    case Action.change => "UPDATE"
    case Action.remove => "DELETE FROM"
  }

  private[models] def aargs(action : Action.Value)(implicit site : Site) : SQLTerms =
    SQLTerms('audit_user -> site.identity.id, 'audit_ip -> site.clientIP, 'audit_action -> action)

  /** Record an audit event of the specified type to the generic audit table. */
  def action(action : Action.Value, table : String = "audit", args : SQLTerms = SQLTerms())(implicit site : Site, dbc : Site.DB, exc : ExecutionContext) : SQL.Result =
    (sql"INSERT INTO audit." + table ++ (aargs(action) ++ args).insert).run

  def actionFor(action : Action.Value, user : Party.Id, ip : Inet)(implicit dbc : Site.DB, exc : ExecutionContext) : SQL.Result =
    (sql"INSERT INTO audit.audit" ++ SQLTerms('audit_user -> user, 'audit_ip -> ip, 'audit_action -> action).insert).run

  private[this] def SQLon(action : Action.Value, table : String, stmt : Statement, returning : String = "")(implicit site : Site, dbc : Site.DB, exc : ExecutionContext) : SQL.Result =
    (lsql"WITH audit_row AS (" + acmd(action) + " " + table ++ stmt + " RETURNING *) INSERT INTO audit." + table + " SELECT CURRENT_TIMESTAMP," ++ aargs(action).join(",") + ", * FROM audit_row" + Maybe.bracket(" RETURNING ", returning))
    .run

  /** Record and perform an [[Action.add]] event for a particular table.
    * This does the equivalent of `INSERT INTO table args VALUES args [RETURNING returning]`.
    * @param table the name of the affected table, with a corresponding "audit_table" table
    * @param args parameters for attached row data
    * @param returning optional values to return from the query. It must not reference the original table explicitly as it is evaluated on the audit table.
    */
  private[models] def add(table : String, args : SQLTerms, returning : String = "")(implicit site : Site, dbc : Site.DB, exc : ExecutionContext) : SQL.Result =
    SQLon(Action.add, table, args.insert, returning)

  /** Record an [[Action.remove]] event to a particular audit table.
    * This does the equivalent of `DELETE FROM table WHERE args [RETURNING returning]`.
    * @param table the name of the affected table, with a corresponding "audit_table" table
    * @param args parameters to select attached row data
    * @param returning optional values to return from the query. It must not reference the original table explicitly as it is evaluated on the audit table.
    */
  private[models] def remove(table : String, args : SQLTerms, returning : String = "")(implicit site : Site, dbc : Site.DB, exc : ExecutionContext) : SQL.Result =
    SQLon(Action.remove, table, "WHERE " +: args.where, returning)

  /** Record an [[Action.change]] event to a particular audit table.
    * This does the equivalent of `UPDATE table SET sets WHERE where [RETURNING returning]`.
    * @param table the name of the affected table, with a corresponding "audit_table" table
    * @param sets parameters to update attached row data
    * @param where parameters to select attached row data
    * @param returning optional values to return from the query. It must not reference the original table explicitly as it is evaluated on the audit table.
    */
  private[models] def change(table : String, sets : SQLTerms, where : SQLTerms, returning : String = "")(implicit site : Site, dbc : Site.DB, exc : ExecutionContext) : SQL.Result =
    if (sets.isEmpty) SQL.Result.empty
    else SQLon(Action.change, table, ("SET " +: sets.set()) ++ (" WHERE " +: where.where), returning)

  private[models] def changeOrAdd(table : String, sets : SQLTerms, ids : SQLTerms)(implicit site : Site, dbc : Site.DB, exc : ExecutionContext) : SQL.Result =
    DBUtil.updateOrInsert(change(table, sets, ids)(site, _, _))(add(table, sets ++ ids)(site, _, _))(dbc, exc)

  private[models] def download(table : String, ids : SQLTerm[_]*)(implicit site : Site, dbc : Site.DB, exc : ExecutionContext) : Future[Boolean] =
    action(Action.open, table, SQLTerms(ids : _*)).execute
}
