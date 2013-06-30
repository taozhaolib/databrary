package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

final case class Authorize(childId : Int, parentId : Int, access : Permission.Value, delegate : Permission.Value, authorized : Option[Timestamp], expires : Option[Timestamp]) extends TableRow {
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

  private[this] val _child = CachedVal[Identity, Site.DB](Identity.get(childId)(_))
  def child(implicit db : Site.DB) : Identity = _child
  private[this] val _parent = CachedVal[Identity, Site.DB](Identity.get(parentId)(_))
  def parent(implicit db : Site.DB) : Identity = _parent
}

object Authorize extends TableView("authorize") {
  private[this] val row = Anorm.rowMap(Authorize.apply _, "child", "parent", "access", "delegate", "authorized", "expires")

  private[this] def select(all : Boolean) = 
    "SELECT * FROM " + table + (if (all) "" else "_valid")

  def get(c : Int, p : Int)(implicit db : Site.DB) : Option[Authorize] =
    SQL(select(true) + " WHERE child = {child} AND parent = {parent}").
      on('child -> c, 'parent -> p).singleOpt(row)

  private[models] def getParents(c : Int, all : Boolean)(implicit db : Site.DB) =
    SQL(select(all) + " WHERE child = {child}").
      on('child -> c).list(row)
  private[models] def getChildren(p : Int, all : Boolean)(implicit db : Site.DB) =
    SQL(select(all) + " WHERE parent = {parent}").
      on('parent -> p).list(row)

  private def delete(c : Int, p : Int)(implicit site : Site) =
    Audit.SQLon(AuditAction.remove, "authorize", "WHERE child = {child} AND parent = {parent}")('child -> c, 'parent -> p).
      execute()(site.db)

  def access_check(c : Int)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT authorize_access_check({id})").
      on('id -> c).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)

  def delegate_check(c : Int, p : Int)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT authorize_delegate_check({child}, {parent})").
      on('child -> c, 'parent -> p).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)
}
