package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._
import scala.slick.ast.{Node,ProductNode}
import scala.slick.driver.BasicProfile
import scala.slick.lifted.{AbstractTable,ColumnBase}
import scala.slick.session.{PositionedResult,PositionedParameters}
import java.sql.Timestamp
import anorm._
import dbrary._
import util._

case class Audit[T](who : Int, ip : Inet, action : AuditAction.Value, row : T) {
  def entity(implicit db : Session) = Identity.get(who)
}

object Audit {
  private[this] def acmd(action : AuditAction.Value) = action match {
    case AuditAction.add => "INSERT INTO"
    case AuditAction.change => "UPDATE"
    case AuditAction.remove => "DELETE FROM"
  }

  private[this] def aargs(action : AuditAction.Value)(implicit site : Site) : List[(Symbol, ParameterValue[_])] =
    List('audit_identity -> site.identity.id, 'audit_ip -> site.clientIP, 'audit_action -> action)

  def SQLon(action : AuditAction.Value, table : String, stmt : String, returning : String = "")(args : (Symbol, ParameterValue[_])*)(implicit site : Site) =
    SQL("WITH audit_row AS (" + acmd(action) + " " + table + " " + stmt + " RETURNING *) INSERT INTO audit_" + table + " SELECT CURRENT_TIMESTAMP, {audit_identity}, {audit_ip}, {audit_action}, * FROM audit_row" + maybe(returning).fold("")(" RETURNING " + _)).on(args ++ aargs(action) : _*)

  def add(action : AuditAction.Value)(implicit site : Site) =
    SQL("INSERT INTO audit (who, ip, action) VALUES ({audit_identity}, {audit_ip}, {audit_action}").on(aargs(action) : _*)
}
