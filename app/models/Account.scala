package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._
import java.sql.Timestamp

case class Account(id : Int, username : String, var email : String, var openid : Option[String]) extends TableRow {
  var entity : Entity = null
  var access : SitePermission.Value = null

  def commit = DB.withSession { implicit session =>
    Account.byId(id).map(_.mutable) update (email, openid)
  }
}

object Account extends Table[Account]("account") {
  def id = column[Int]("entity")
  def username = column[String]("username", O.PrimaryKey, O.DBType("varchar(32)"))
  def created = column[Timestamp]("created")
  def email = column[String]("email", O.DBType("varchar(256)"))
  def openid = column[Option[String]]("openid", O.DBType("varchar(256)"))

  def * = id ~ username ~ email ~ openid <> (Account.apply _, Account.unapply _)
  def mutable = email ~ openid

  def idKey = index("account_entity_key", id, unique = true)
  def entity = foreignKey("account_entity_fkey", id, Entity)(_.id)

  def byId(i : Int) = Query(this).where(_.id === i)
  def byUsername(u : String) = Query(this).filter(_.username === u)

  private def fillEntity(ae : (Account,Entity,Option[SitePermission.Value])) : Account =
    ae match { case (a,e,c) => a.entity = e; a.access = c.getOrElse(SitePermission.NONE); a }
  private def withEntity(q : Query[Account.type, Account]) : Query[(Account.type,Entity.type,Column[Option[SitePermission.Value]]), (Account,Entity,Option[SitePermission.Value])] =
    for { a <- q ; (e, c) <- a.entity.map(e => (e, Trust._check(e.id))) } yield (a,e,c)

  def getId(i : Int) : Option[Account] = DB.withSession { implicit session =>
    withEntity(byId(i)).firstOption.map(fillEntity _)
  }
  def getUsername(u : String) : Option[Account] = DB.withSession { implicit session =>
    withEntity(byUsername(u)).firstOption.map(fillEntity _)
  }
}
