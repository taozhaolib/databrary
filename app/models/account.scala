package models

import anorm._
import dbrary._
import util._

private[models] final class Account (val id : Entity.Id, val username : String, email_ : String, openid_ : Option[String]) extends TableRowId(id.unId) {
  private[this] var _email = email_
  def email = _email
  private[this] var _openid = openid_
  def openid = _openid

  def change(email : String = _email, openid : Option[String] = _openid)(implicit site : Site) : Unit = {
    if (email == _email && openid == _openid)
      return
    Audit.SQLon(AuditAction.change, Account.table, "SET email = {email}, openid = {openid} WHERE id = {id}")('email -> email, 'openid -> openid, 'id -> id).execute()(site.db)
    _email = email
    _openid = openid
  }
}

private[models] object Account extends TableView[Account]("account") {
  private[this] def make(id : Entity.Id, username : String, email : String, openid : Option[String]) =
    new Account(id, username, email, openid)
  private[models] val row = Anorm.rowMap(make _, "id", "username", "email", "openid")
}
