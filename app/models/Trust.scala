package models

import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._
import java.sql.Timestamp

object SitePermission extends DBEnum("site_permission") {
  val NONE, BROWSE, FULL, GRANT = Value
}

object UserPermission extends DBEnum("user_permission") {
  val NONE, VIEW, EDIT, ADMIN = Value
}

case class Trust(child : Int, parent : Int, var access : SitePermission.Value, var delegate : UserPermission.Value, var expires : Option[Timestamp]) extends TableRow {
  lazy val childEntity : Entity = Entity.get(child)
  lazy val parentEntity : Entity = Entity.get(parent)

  def commit = DB.withSession { implicit session =>
    Trust.byKey(child, parent).map(_.mutable) update (access, delegate, expires)
  }
  def add = DB.withSession { implicit session =>
    Trust.* insert this
  }
  def remove = DB.withSession { implicit session =>
    Trust.byKey(child, parent).delete
  }
}

object Trust extends Table[Trust]("trust") {
  def child = column[Int]("child")
  def parent = column[Int]("parent")
  def access = column[SitePermission.Value]("access")
  def delegate = column[UserPermission.Value]("delegate")
  def authorized = column[Timestamp]("authorized")
  def expires = column[Option[Timestamp]]("expires")

  def * = child ~ parent ~ access ~ delegate ~ expires <> (Trust.apply _, Trust.unapply _)
  def mutable = access ~ delegate ~ expires

  def key = primaryKey("trust_pkey", (child, parent))
  def childEntity = foreignKey("trust_child_fkey", child, Entity)(_.id)
  def parentEntity = foreignKey("trust_parent_fkey", parent, Entity)(_.id)

  def byKey(c : Int, p : Int) = Query(this).where(r => r.child === c && r.parent === p)
  def get(c : Int, p : Int) : Option[Trust] = DB.withSession { implicit session =>
    byKey(c, p).firstOption
  }
  def getParents(c : Int) : List[Trust] = DB.withSession { implicit session =>
    Query(this).where(_.child === c).list
  }
  def getChildren(p : Int) : List[Trust] = DB.withSession { implicit session =>
    Query(this).where(_.parent === p).list
  }

  val _check = SimpleFunction.ternary[Int, Int, Option[SitePermission.Value], Option[SitePermission.Value]]("trust_check")
  def check(c : Int, p : Int = Entity.ROOT) : SitePermission.Value = DB.withSession { implicit session =>
    Query(_check(c, p, None)).first.getOrElse(SitePermission.NONE)
  }
}
