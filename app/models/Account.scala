package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple._
import java.sql.Timestamp

private[models] final case class Account(id : Int, username : String, var email : String, var openid : Option[String]) 
  extends TableRow {
  def commit(implicit db : Session) =
    Account.byId(id).map(_.update_*) update (email, openid)
  def add(implicit db : Session) =
    Account.* insert this
}

private[models] object Account extends Table[Account]("account") {
  def id = column[Int]("entity")
  def username = column[String]("username", O.PrimaryKey, O.DBType("varchar(32)"))
  def created = column[Timestamp]("created")
  def email = column[String]("email", O.DBType("varchar(256)"))
  def openid = column[Option[String]]("openid", O.DBType("varchar(256)"))

  def * = id ~ username ~ email ~ openid <> (Account.apply _, Account.unapply _)
  def ? = id.? ~ username.? ~ email.? ~ openid <> (
    (id, username, email, openid) => id.map(Account(_, username.get, email.get, openid)),
    (x : Option[Account]) => x.map({ case Account(id, username, email, openid) => (Some(id), Some(username), Some(email), openid) })
  )
  private def update_* = email ~ openid

  def idKey = index("account_entity_key", id, unique = true)
  def openidKey = index("account_openid_key", openid, unique = false)
  def entity = foreignKey("account_entity_fkey", id, Entity)(_.id)

  def byId(i : Int) = Query(this).where(_.id === i)
  def byUsername(u : String) = Query(this).filter(_.username === u)
  def byOpenid(o : String) = Query(this).filter(_.openid === o)
}
