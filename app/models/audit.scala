package models

import play.api.Play.current
import anorm._
import dbrary._
import util._

object AuditAction extends PGEnum("audit_action") {
  val login, logout, add, change, remove, download = Value
}

case class Audit[T](who : Party.Id, ip : Inet, action : AuditAction.Value, row : T) extends TableRow {
  private val _party = CachedVal[Option[Party], Site](Party.get(who)(_))
  def party(implicit site : Site) : Option[Party] = _party
}

object Audit {
  private[this] def acmd(action : AuditAction.Value) = action match {
    case AuditAction.add => "INSERT INTO"
    case AuditAction.change => "UPDATE"
    case AuditAction.remove => "DELETE FROM"
  }

  private[this] def aargs(action : AuditAction.Value)(implicit site : Site) : SQLArgs =
    SQLArgs('identity -> site.identity.id, 'audit_ip -> site.clientIP, 'audit_action -> action)

  def action(action : AuditAction.Value)(implicit site : Site) =
    SQL("INSERT INTO audit (who, ip, action) VALUES ({identity}, {audit_ip}, {audit_action}").on(aargs(action) : _*)

  private[this] def SQLon(action : AuditAction.Value, table : String, stmt : String, returning : String = "")(args : SQLArgs)(implicit site : Site) =
    SQL("WITH audit_row AS (" + acmd(action) + " " + table + " " + stmt + " RETURNING *) INSERT INTO audit_" + table + " SELECT CURRENT_TIMESTAMP, {identity}, {audit_ip}, {audit_action}, * FROM audit_row" + maybe(returning).fold("")(" RETURNING " + _)).on(args ++ aargs(action) : _*)

  def add(table : String, args : SQLArgs, returning : String = "")(implicit site : Site) =
    SQLon(AuditAction.add, table, args.insert, returning)(args)(site)

  def remove(table : String, args : SQLArgs, returning : String = "")(implicit site : Site) =
    SQLon(AuditAction.remove, table, "WHERE " + args.set(" AND "), returning)(args)(site)

  def change(table : String, sets : SQLArgs, where : SQLArgs, returning : String = "")(implicit site : Site) =
    SQLon(AuditAction.change, table, "SET " + sets.set(", ") + " WHERE " + where.set(" AND "), returning)(SQLArgs(sets ++ where : _*))(site)
}
