package models

import anorm._
import dbrary._
import util._

/* Account refines Entity for individuals with registered (but not necessarily authorized) accounts on the site.
 * This is internal. The external interface to Account is User.
 */

private[models] final class Account (val id : Entity.Id, val username : String, email_ : String, openid_ : Option[String], timezone_ : Option[String]) extends TableRowId[Entity] {
  private[this] var _email = email_
  def email = _email
  private[this] var _openid = openid_
  def openid = _openid
  private[this] var _timezone = timezone_
  def timezone = _timezone

  def change(email : String = _email, openid : Option[String] = _openid, timezone : Option[String] = _timezone)(implicit site : Site) : Unit = {
    if (email == _email && openid == _openid && timezone == _timezone)
      return
    Audit.SQLon(AuditAction.change, Account.table, "SET email = {email}, openid = {openid}, timezone = {timezone} WHERE id = {id}")(
      'email -> email, 'openid -> openid, 'timezone -> timezone, 'id -> id).execute()(site.db)
    _email = email
    _openid = openid
    _timezone = timezone
  }
}

private[models] object Account extends TableView[Account]("account") {
  private[this] def make(id : Entity.Id, username : String, email : String, openid : Option[String], timezone : Option[String]) =
    new Account(id, username, email, openid, timezone)
  private[models] val row = Anorm.rowMap(make _, "id", col("username"), col("email"), col("openid"), col("timezone"))
}
