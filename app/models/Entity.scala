package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._

case class Entity(id : Int, var name : String) extends TableRow {
  def commit = DB.withSession { implicit session =>
    Entity.byId(id).map(_.mutable).update(name)
  }
}

object Entity extends Table[Entity]("entity") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.DBType("text"))

  def * = id ~ name <> (Entity.apply _, Entity.unapply _)
  def mutable = name

  def byId(i : Int) = Query(this).filter(_.id === i)
  def get(i : Int) : Entity = DB.withSession { implicit session =>
    byId(i).firstOption.orNull
  }
  def create(n : String) : Entity = DB.withSession { implicit session =>
    val i = name returning id insert n
    Entity(i, n)
  }

  val ROOT : Int = 0
}
