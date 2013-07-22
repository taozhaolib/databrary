package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

object Permission extends PGEnum("permission") {
  val NONE, VIEW, DOWNLOAD, CONTRIBUTE, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  def OWN = ADMIN
  def EDIT = CONTRIBUTE
  def DATA = DOWNLOAD
  def COMMENT = VIEW
}

final case class Authorize(childId : Identity.Id, parentId : Identity.Id, access : Permission.Value, delegate : Permission.Value, authorized : Option[Timestamp], expires : Option[Timestamp]) extends TableRow {
  private def id =
    Anorm.Args('child -> childId, 'parent -> parentId)
  private def args =
    id ++ Anorm.Args('access -> access, 'delegate -> delegate, 'authorized -> authorized, 'expires -> expires)

  def set(implicit site : Site) : Unit = {
    val args = this.args
    if (Audit.SQLon(AuditAction.change, Authorize.table, "SET access = {access}, delegate = {delegate}, authorized = {authorized}, expires = {expires} WHERE child = {child} AND parent = {parent}")(args : _*).executeUpdate()(site.db) == 0)
      Audit.SQLon(AuditAction.add, Authorize.table, Anorm.insertArgs(args))(args : _*).execute()(site.db)
  }
  def remove(implicit site : Site) : Unit =
    Authorize.delete(childId, parentId)

  def valid = {
    val now = (new java.util.Date).getTime
    authorized.fold(false)(_.getTime < now) && expires.fold(true)(_.getTime > now)
  }

  private[Authorize] val _child = CachedVal[Identity, Site.DB](Identity.get(childId)(_).get)
  def child(implicit db : Site.DB) : Identity = _child
  private[Authorize] val _parent = CachedVal[Identity, Site.DB](Identity.get(parentId)(_).get)
  def parent(implicit db : Site.DB) : Identity = _parent
}

object Authorize extends TableView[Authorize]("authorize") {
  private[models] val row = Anorm.rowMap(Authorize.apply _, "child", "parent", "access", "delegate", "authorized", "expires")

  private[this] def select(all : Boolean) = 
    "SELECT * FROM " + src + (if (all) "" else "_valid")

  def get(c : Identity.Id, p : Identity.Id)(implicit db : Site.DB) : Option[Authorize] =
    SQL(select(true) + " WHERE child = {child} AND parent = {parent}").
      on('child -> c, 'parent -> p).singleOpt(row)

  private[models] def getParents(c : Identity, all : Boolean)(implicit db : Site.DB) =
    SQL(select(all) + " WHERE child = {child}").
      on('child -> c.id).list(
        row map { a => a._child() = c ; a }
      )
  private[models] def getChildren(p : Identity, all : Boolean)(implicit db : Site.DB) =
    SQL(select(all) + " WHERE parent = {parent}").
      on('parent -> p.id).list(
        row map { a => a._parent() = p ; a }
      )

  def delete(c : Identity.Id, p : Identity.Id)(implicit site : Site) =
    Audit.SQLon(AuditAction.remove, "authorize", "WHERE child = {child} AND parent = {parent}")('child -> c, 'parent -> p).
      execute()(site.db)

  private[models] def access_check(c : Identity.Id)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT authorize_access_check({id})").
      on('id -> c).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)

  private[models] def delegate_check(c : Identity.Id, p : Identity.Id)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT authorize_delegate_check({child}, {parent})").
      on('child -> c, 'parent -> p).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)
}
