package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._
import util._

private[models] final class Entity(val id : Int, name_ : String, orcid_ : Option[Orcid] = None) extends TableRow {
  override def hashCode = id
  override def equals(e : Any) = e match {
    case e : Entity => e.id == id
    case _ => false
  }

  private[this] var _name = name_
  def name = _name
  private[this] var _orcid = orcid_
  def orcid = _orcid

  private def * = (id, _name, _orcid)

  def change(name : String = _name, orcid : Option[Orcid] = _orcid)(implicit site : Site) : Unit = {
    if (name == _name && orcid == _orcid)
      return
    implicit val db = site.db
    Entity.byId(id).map(_.update_*) update (name, orcid)
    _name = name
    _orcid = orcid
    AuditEntity.add(AuditAction.change, this)
  }

  private[this] val _access = CachedVal[Permission.Value](Authorize.access_check(id)(_))
  def access(implicit db : Session) : Permission.Value = _access
}

private[models] object Entity extends Table[Entity]("entity") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.DBType("text"))
  def orcid = column[Option[Orcid]]("orcid", O.DBType("char(16)"))

  def apply(id : Int, name : String, orcid : Option[Orcid]) =
    new Entity(id, name, orcid)
  def unapply(e : Entity) = Some(e.*)

  def * = id ~ name ~ orcid <> (apply _, unapply _)
  private def update_* = name ~ orcid
  private[this] def insert_* = name

  private def byId(i : Int) = Query(this).where(_.id === i)

  def create(n : String)(implicit db : Session) : Entity =
    new Entity(insert_* returning id insert n, n)

  final val NOBODY : Int = -1
  final val ROOT   : Int = 0
  final val Nobody = new Entity(NOBODY, "Everybody")
  final val Root   = new Entity(ROOT,   "Databrary")
}

object AuditEntity extends AuditTable[Entity](Entity) {
  def id = column[Int]("id")
  def name = column[String]("name")
  def orcid = column[Option[Orcid]]("orcid")
  def row = id ~ name ~ orcid <> (Entity.apply _, Entity.unapply _)
}
