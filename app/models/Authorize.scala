package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple._
import java.sql.Timestamp

object Permission extends DBEnum("permission") {
  val NONE, VIEW, DOWNLOAD, CONTRIBUTE, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  def EDIT = CONTRIBUTE
  def DATA = DOWNLOAD
  def OWN = ADMIN
}

final case class Authorize(child : Int, parent : Int, var access : Permission.Value, var delegate : Permission.Value, var authorized : Option[Timestamp], var expires : Option[Timestamp]) extends TableRow {
  var id = (child, parent)
  def commit = DB.withSession { implicit session =>
    Authorize.byKey(child, parent).map(_.update_*) update (access, delegate, authorized, expires)
  }
  def add = DB.withSession { implicit session =>
    Authorize.* insert this
  }
  def remove =
    Authorize.delete(child, parent)

  lazy val childEntity : Entity = Entity.get(child)
  lazy val parentEntity : Entity = Entity.get(parent)
}

object Authorize extends Table[Authorize]("authorize") {
  def child = column[Int]("child")
  def parent = column[Int]("parent")
  def access = column[Permission.Value]("access")
  def delegate = column[Permission.Value]("delegate")
  def authorized = column[Option[Timestamp]]("authorized")
  def expires = column[Option[Timestamp]]("expires")

  def id = child ~ parent
  def * = child ~ parent ~ access ~ delegate ~ authorized ~ expires <> (Authorize.apply _, Authorize.unapply _)
  private def update_* = access ~ delegate ~ authorized ~ expires

  def key = primaryKey("authorize_pkey", (child, parent))
  def childEntity = foreignKey("authorize_child_fkey", child, Entity)(_.id)
  def parentEntity = foreignKey("authorize_parent_fkey", parent, Entity)(_.id)

  private def byKey(c : Int, p : Int) = Query(this).where(t => t.child === c && t.parent === p)
  def byChild(c : Int) = Query(this).where(_.child === c).sortBy(_.authorized.nullsLast)
  def byParent(p : Int) = Query(this).where(_.parent === p).sortBy(_.authorized.nullsLast)

  private[this] def _valid(a : Authorize.type) = 
    a.authorized <= DBFunctions.currentTimestamp && (a.expires.isNull || a.expires > DBFunctions.currentTimestamp)

  def get(c : Int, p : Int) : Option[Authorize] = DB.withSession { implicit session =>
    byKey(c, p).firstOption
  }
  def getParents(c : Int, all : Boolean) : List[Authorize] = DB.withSession { implicit session =>
    val l = byChild(c)
    (if (all) l else l.filter(_valid(_))).list
  }
  def getChildren(p : Int, all : Boolean) : List[Authorize] = DB.withSession { implicit session =>
    val l = byParent(p)
    (if (all) l else l.filter(_valid(_))).list
  }
  def delete(c : Int, p : Int) = DB.withSession { implicit session =>
    byKey(c, p).delete
  }

  val _access_check = SimpleFunction.unary[Int, Option[Permission.Value]]("authorize_access_check")
  def access_check(c : Int) : Permission.Value = DB.withSession { implicit session =>
    Query(_access_check(c)).first.getOrElse(Permission.NONE)
  }

  private[this] val _delegate_check = SimpleFunction.binary[Int, Int, Option[Permission.Value]]("authorize_delegate_check")
  def delegate_check(c : Int, p : Int) : Permission.Value = DB.withSession { implicit session =>
    Query(_delegate_check(c, p)).first.getOrElse(Permission.NONE)
  }
}
