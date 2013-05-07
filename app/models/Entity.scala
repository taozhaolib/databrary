package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._
import collection.mutable.HashMap

case class Entity(id : Int, var name : String) extends TableRow {
  override def hashCode = id
  override def equals(e : Any) = e match {
    case Entity(i, _) => i == id
    case _ => false
  }

  def commit = DB.withSession { implicit session =>
    Entity.byId(id).map(_.mutable) update (name)
  }

  def account = Account.getId(id)
  private val _access = CachedVal[SitePermission.Value](Trust.access_check(id))
  def access : SitePermission.Value = _access
  def trustParents = Trust.getParents(id)
  def trustChildren = Trust.getChildren(id)
}

private object EntityCache extends HashMap[Int, Entity]

object Entity extends Table[Entity]("entity") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.DBType("text"))

  def * = id ~ name <> (Entity.apply _, Entity.unapply _)
  def mutable = name

  private def byId(i : Int) = Query(this).where(_.id === i)

  def cache(e : Entity, a : SitePermission.Value = null) : Entity = {
    e._access() = a
    EntityCache.put(e.id, e)
    e
  }
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

  def byName(n : String) = {
    // should clearly be improved and/or indexed
    val w = "%" + n.split("\\s+").filter(!_.isEmpty).mkString("%") + "%"
    for {
      (e, a) <- Entity leftJoin Account on (_.id === _.id)
      if a.username === n || DBFunctions.ilike(e.name, w)
    } yield e
  }

  final val ROOT : Int = 0
}
