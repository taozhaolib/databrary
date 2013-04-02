package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._

case class Entity(id : Int, name : String) {
  def name_=(n : String) = DB.withSession { implicit session =>
    (for { e <- Entity if e.id === id } yield e.name).update(n)
  }
}

object Entity extends Table[Entity]("entity") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.DBType("text"))

  def * = id ~ name <> (Entity.apply _, Entity.unapply _)

  val byId = createFinderBy(_.id)
  def get(id : Int) : Option[Entity] = DB.withSession { implicit session =>
    byId(id).firstOption
  }
}
