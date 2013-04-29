package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._
import collection.mutable.HashMap

case class Entity(id : Int, var name : String) extends TableRow {
  def commit = DB.withSession { implicit session =>
    Entity.byId(id).map(_.mutable) update (name)
  }

  lazy val siteAccess : SitePermission.Value = Trust.check(id)
}

object EntityCache extends HashMap[Int, Entity]

object Entity extends Table[Entity]("entity") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.DBType("text"))

  def * = id ~ name <> (Entity.apply _, Entity.unapply _)
  def mutable = name

  def byId(i : Int) = Query(this).where(_.id === i)

  def get(i : Int) : Entity =
    EntityCache.getOrElseUpdate(i,
      DB.withSession { implicit session =>
        byId(i).firstOption.orNull
      })
  def create(n : String) : Entity = {
    val i = DB.withSession { implicit session =>
      name returning id insert n
    }
    val e = Entity(i, n)
    EntityCache.update(i, e)
    e
  }

  val ROOT : Int = 0
}
