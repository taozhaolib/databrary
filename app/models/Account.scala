package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._
import java.sql.Timestamp

case class Account(id : Int, username : String, var email : String) extends TableRow {
  var entity : Entity = null

  def commit = DB.withSession { implicit session =>
    Account.byId(id).map(_.mutable) update (email)
  }
}

object Account extends Table[Account]("account") {
  def id = column[Int]("entity")
  def username = column[String]("username", O.PrimaryKey, O.DBType("varchar(32)"))
  def email = column[String]("email", O.DBType("varchar(256)"))
  def created = column[Timestamp]("created")

  def * = id ~ username ~ email <> (Account.apply _, Account.unapply _)
  def mutable = email

  def idKey = index("account_entity_key", id, unique = true)
  def entity = foreignKey("account_entity_fkey", id, Entity)(_.id)

  def byId(i : Int) = Query(this).where(_.id === i)
  def byUsername(u : String) = Query(this).filter(_.username === u)

  private def fillEntity(ae : (Account,Entity)) : Account =
    ae match { case (a,e) => a.entity = e; a }
  private def withEntity(q : Query[Account.type, Account]) : Query[(Account.type,Entity.type), (Account,Entity)] =
    for { a <- q ; e <- a.entity } yield (a,e)

  def getId(i : Int) : Option[Account] = DB.withSession { implicit session =>
    withEntity(byId(i)).firstOption.map(fillEntity _)
  }
  def getUsername(u : String) : Option[Account] = DB.withSession { implicit session =>
    withEntity(byUsername(u)).firstOption.map(fillEntity _)
  }
}
