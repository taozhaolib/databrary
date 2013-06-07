package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple._
import java.sql.Timestamp
import util._

private[models] final class Account(val id : Int, val username : String, email_ : String, openid_ : Option[String]) extends TableRow {
  override def hashCode = id
  override def equals(a : Any) = a match {
    case Account(i, _, _, _) => i == id
    case _ => false
  }

  private[this] var _email = email_
  def email = _email
  private[this] var _openid = openid_
  def openid = _openid

  private def * = (id, username, _email, _openid)

  def change(email : String = _email, openid : Option[String] = _openid)(implicit site : Site) : Unit = {
    if (email == _email && openid == _openid)
      return
    implicit val db = site.db
    Account.byId(id).map(_.update_*) update (email, openid)
    _email = email
    _openid = openid
    AuditAccount.add(AuditAction.change, this)
  }
}

private[models] object Account extends Table[Account]("account") {
  def id = column[Int]("entity")
  def username = column[String]("username", O.PrimaryKey, O.DBType("varchar(32)"))
  def created = column[Timestamp]("created")
  def email = column[String]("email", O.DBType("varchar(256)"))
  def openid = column[Option[String]]("openid", O.DBType("varchar(256)"))

  def apply(id : Int, username : String, email : String, openid : Option[String]) =
    new Account(id, username, email, openid)
  def unapply(a : Account) = Some(a.*)

  def * = id ~ username ~ email ~ openid <> (apply _, unapply _)
  def ? = id.? ~ username.? ~ email.? ~ openid <> (
    (id, username, email, openid) => id.map(Account(_, username.get, email.get, openid)),
    (x : Option[Account]) => x.map({ case Account(id, username, email, openid) => (Some(id), Some(username), Some(email), openid) })
  )
  private def update_* = email ~ openid

  private[this] def idKey = index("account_entity_key", id, unique = true)
  private[this] def openidKey = index("account_openid_key", openid, unique = false)
  def entity = foreignKey("account_entity_fkey", id, Entity)(_.id)

  private def byId(i : Int) = Query(this).where(_.id === i)
}

object AuditAccount extends AuditTable[Account](Account) {
  def id = column[Int]("entity")
  def username = column[String]("username")
  def created = column[Timestamp]("created")
  def email = column[String]("email")
  def openid = column[Option[String]]("openid")

  def row = id ~ username ~ email ~ openid <> (Account.apply _, Account.unapply _)
}
