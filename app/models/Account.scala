package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple._
import java.sql.Timestamp
import anorm._
import dbrary._
import util._

private[models] final class Account (val id : Int, val username : String, email_ : String, openid_ : Option[String]) extends TableRow {
  override def hashCode = id
  def equals(a : Account) = a.id == id

  private[this] var _email = email_
  def email = _email
  private[this] var _openid = openid_
  def openid = _openid

  def change(email : String = _email, openid : Option[String] = _openid)(implicit site : Site) : Unit = {
    if (email == _email && openid == _openid)
      return
    implicit val db = site.db.conn
    Audit.SQLon(AuditAction.change, Account.table, "SET email = {email}, openid = {openid} WHERE id = {id}")('email -> email, 'openid -> openid, 'id -> id).execute()
    _email = email
    _openid = openid
  }
}

private[models] object Account extends TableView("account") {
}
