package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple._
import java.sql.Timestamp
import util._

object Permission extends DBEnum("permission") {
  val NONE, VIEW, DOWNLOAD, CONTRIBUTE, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  def EDIT = CONTRIBUTE
  def DATA = DOWNLOAD
  def OWN = ADMIN
}

final case class Authorize(childId : Int, parentId : Int, access : Permission.Value, delegate : Permission.Value, authorized : Option[Timestamp], expires : Option[Timestamp]) extends TableRow {
  val id = (childId, parentId)

  def set(implicit site : Site) : Unit = {
    implicit val db = site.db
    val r = Authorize.byId(id).map(_.update_*) update (access, delegate, authorized, expires)
    val act = if (r == 0) {
      /* possible race condition; should be done the other way but catching is too annoying */
      Authorize.* insert this
      AuditAction.add
    } else
      AuditAction.change
    AuditAuthorize.add(act, this)
  }
  def remove(implicit site : Site) : Unit = {
    implicit val db = site.db
    val r = Authorize.delete(id)
    if (r > 0)
      AuditAuthorize.add(AuditAction.remove, this)
  }

  private[this] val _child = CachedVal[Identity](Identity.get(childId)(_))
  def child(implicit db : Session) : Identity = _child
  private[this] val _parent = CachedVal[Identity](Identity.get(parentId)(_))
  def parent(implicit db : Session) : Identity = _parent
}

object Authorize extends Table[Authorize]("authorize") {
  def child = column[Int]("child")
  def parent = column[Int]("parent")
  def access = column[Permission.Value]("access")
  def delegate = column[Permission.Value]("delegate")
  def authorized = column[Option[Timestamp]]("authorized")
  def expires = column[Option[Timestamp]]("expires")

  type Id = (Int,Int)
  def id = child ~ parent
  def * = child ~ parent ~ access ~ delegate ~ authorized ~ expires <> (apply _, unapply _)
  private def update_* = access ~ delegate ~ authorized ~ expires

  def key = primaryKey("authorize_pkey", (child, parent))
  private[this] def childEntity = foreignKey("authorize_child_fkey", child, Entity)(_.id)
  private[this] def parentEntity = foreignKey("authorize_parent_fkey", parent, Entity)(_.id)

  private def byKey(c : Int, p : Int) = Query(this).where(t => t.child === c && t.parent === p)
  private def byId(i : Id) = byKey(i._1, i._2)
  def byChild(c : Int) = Query(this).where(_.child === c).sortBy(_.authorized.nullsLast)
  def byParent(p : Int) = Query(this).where(_.parent === p).sortBy(_.authorized.nullsLast)

  private[this] def _valid(a : Authorize.type) = 
    a.authorized <= DBFunctions.currentTimestamp && (a.expires.isNull || a.expires > DBFunctions.currentTimestamp)

  def get(c : Int, p : Int)(implicit db : Session) : Option[Authorize] =
    byKey(c, p).firstOption
  def getParents(c : Int, all : Boolean)(implicit db : Session) : List[Authorize] = {
    val l = byChild(c)
    (if (all) l else l.filter(_valid(_))).list
  }
  def getChildren(p : Int, all : Boolean)(implicit db : Session) : List[Authorize] = {
    val l = byParent(p)
    (if (all) l else l.filter(_valid(_))).list
  }

  private def delete(i : Id)(implicit db : Session) =
    byId(i).delete

  val _access_check = SimpleFunction.unary[Int, Option[Permission.Value]]("authorize_access_check")
  def access_check(c : Int)(implicit db : Session) : Permission.Value =
    Query(_access_check(c)).first.getOrElse(Permission.NONE)

  private[this] val _delegate_check = SimpleFunction.binary[Int, Int, Option[Permission.Value]]("authorize_delegate_check")
  def delegate_check(c : Int, p : Int)(implicit db : Session) : Permission.Value =
    Query(_delegate_check(c, p)).first.getOrElse(Permission.NONE)
}

object AuditAuthorize extends AuditTable[Authorize](Authorize) {
  def child = column[Int]("child")
  def parent = column[Int]("parent")
  def access = column[Permission.Value]("access")
  def delegate = column[Permission.Value]("delegate")
  def authorized = column[Option[Timestamp]]("authorized")
  def expires = column[Option[Timestamp]]("expires")

  def row = child ~ parent ~ access ~ delegate ~ authorized ~ expires <> (Authorize.apply _, Authorize.unapply _)
}
