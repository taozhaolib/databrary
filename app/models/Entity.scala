package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._

private[models] final case class Entity(id : Int, var name : String) extends TableRow {
  override def hashCode = id
  override def equals(e : Any) = e match {
    case Entity(i, _) => i == id
    case _ => false
  }

  def commit(implicit db : Session) =
    Entity.byId(id).map(_.update_*) update (name)

  private val _access = CachedVal[Permission.Value](Authorize.access_check(id)(_))
  def access(implicit db : Session) : Permission.Value = _access
}

private[models] object Entity extends Table[Entity]("entity") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.DBType("text"))

  def * = id ~ name <> (Entity.apply _, Entity.unapply _)
  private def update_* = name
  private[this] def insert_* = update_*

  def byId(i : Int) = Query(this).where(_.id === i)

  def create(n : String)(implicit db : Session) : Entity =
    Entity(insert_* returning id insert n, n)

  final val NOBODY : Int = -1
  final val ROOT : Int = 0
}
