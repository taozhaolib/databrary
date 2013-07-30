package models

import play.api.Play.current
import anorm._
import dbrary._
import util._

object AuditAction extends PGEnum("audit_action") {
  val login, logout, add, change, remove, download = Value
}

case class Audit[T](who : Entity.Id, ip : Inet, action : AuditAction.Value, row : T) {
  private val _entity = CachedVal[Option[Entity], Site](Entity.get(who)(_))
  def entity(implicit site : Site) : Option[Entity] = _entity
}

object Audit {
  private[this] def acmd(action : AuditAction.Value) = action match {
    case AuditAction.add => "INSERT INTO"
    case AuditAction.change => "UPDATE"
    case AuditAction.remove => "DELETE FROM"
  }

  private[this] def aargs(action : AuditAction.Value)(implicit site : Site) : List[(Symbol, ParameterValue[_])] =
    List('identity -> site.identity.id, 'audit_ip -> site.clientIP, 'audit_action -> action)

  def SQLon(action : AuditAction.Value, table : String, stmt : String, returning : String = "")(args : (Symbol, ParameterValue[_])*)(implicit site : Site) =
    SQL("WITH audit_row AS (" + acmd(action) + " " + table + " " + stmt + " RETURNING *) INSERT INTO audit_" + table + " SELECT CURRENT_TIMESTAMP, {identity}, {audit_ip}, {audit_action}, * FROM audit_row" + maybe(returning).fold("")(" RETURNING " + _)).on(args ++ aargs(action) : _*)

  def add(action : AuditAction.Value)(implicit site : Site) =
    SQL("INSERT INTO audit (who, ip, action) VALUES ({identity}, {audit_ip}, {audit_action}").on(aargs(action) : _*)
}
