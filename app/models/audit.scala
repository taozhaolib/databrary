package models

import play.api.Play.current
import anorm._
import dbrary._
import util._

/** The possible events or actions on the site that can be put into audit tables.
  * Must match the corresponding postgres "audit_action" type. */
object AuditAction extends PGEnum("audit_action") {
  val login, logout, add, change, remove, download = Value
}

/** Represents an event row in an audit table of a particular type.  Currently unused as there is intentionally no read/modify access to audit tables.
  * @constructor Create a new unpersisted audit record. There is no reason to do this.
  * @tparam T the type of data attached to this record. Usually this is a [[models.TableRow]] instance for a corresponding audit_t table.
  * @param when the time of this event
  * @param who the user who generated this event. May no longer be a valid party.
  * @param ip the ip of the client machine that generated this event
  * @param action the type of event
  * @param row the remaining data columns
  */
case class Audit[T](when : java.sql.Timestamp, who : Party.Id, ip : Inet, action : AuditAction.Value, row : T) extends TableRow {
  private val _party = CachedVal[Option[Party], Site](Party.get(who)(_))
  /** Look up the party who generated this event, if still valid. */
  def party(implicit site : Site) : Option[Party] = _party
}

/** Helper for audit tables.  Not a [[models.TableView]] because it corresponds to multiple underlying tables. */
object Audit {
  private[this] def acmd(action : AuditAction.Value) = action match {
    case AuditAction.add => "INSERT INTO"
    case AuditAction.change => "UPDATE"
    case AuditAction.remove => "DELETE FROM"
  }

  private[this] def aargs(action : AuditAction.Value)(implicit site : Site) : SQLArgs =
    SQLArgs('identity -> site.identity.id, 'audit_ip -> site.clientIP, 'audit_action -> action)

  /** Record an audit event of the specified type to the generic audit table. */
  def action(action : AuditAction.Value)(implicit site : Site) =
    SQL("INSERT INTO audit (who, ip, action) VALUES ({identity}, {audit_ip}, {audit_action}").on(aargs(action) : _*)

  private[this] def SQLon(action : AuditAction.Value, table : String, stmt : String, returning : String = "")(args : SQLArgs)(implicit site : Site) =
    SQL("WITH audit_row AS (" + acmd(action) + " " + table + " " + stmt + " RETURNING *) INSERT INTO audit_" + table + " SELECT CURRENT_TIMESTAMP, {identity}, {audit_ip}, {audit_action}, * FROM audit_row" + maybe(returning).fold("")(" RETURNING " + _)).on(args ++ aargs(action) : _*)

  /** Record and perform an [[AuditAction.add]] event for a particular table.
    * This does the equivalent of `INSERT INTO table args VALUES args [RETURNING returning]`.
    * @param table the name of the affected table, with a corresponding "audit_table" table
    * @param args parameters for attached row data
    * @param returning optional values to return from the query. It must not reference the original table explicitly as it is evaluated on the audit table.
    */
  def add(table : String, args : SQLArgs, returning : String = "")(implicit site : Site) =
    SQLon(AuditAction.add, table, args.insert, returning)(args)(site)

  /** Record an [[AuditAction.remove]] event to a particular audit table.
    * This does the equivalent of `DELETE FROM table WHERE args [RETURNING returning]`.
    * @param table the name of the affected table, with a corresponding "audit_table" table
    * @param args parameters to select attached row data
    * @param returning optional values to return from the query. It must not reference the original table explicitly as it is evaluated on the audit table.
    */
  def remove(table : String, args : SQLArgs, returning : String = "")(implicit site : Site) =
    SQLon(AuditAction.remove, table, "WHERE " + args.set(" AND "), returning)(args)(site)

  /** Record an [[AuditAction.change]] event to a particular audit table.
    * This does the equivalent of `UPDATE table SET sets WHERE where [RETURNING returning]`.
    * @param table the name of the affected table, with a corresponding "audit_table" table
    * @param sets parameters to update attached row data
    * @param where parameters to select attached row data
    * @param returning optional values to return from the query. It must not reference the original table explicitly as it is evaluated on the audit table.
    */
  def change(table : String, sets : SQLArgs, where : SQLArgs, returning : String = "")(implicit site : Site) =
    SQLon(AuditAction.change, table, "SET " + sets.set(", ") + " WHERE " + where.set(" AND "), returning)(SQLArgs(sets ++ where : _*))(site)
}
