package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple._
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
    implicit val db = site.db.conn
    val args = this.args
    if (Audit.SQLon(AuditAction.change, Authorize.table, "SET access = {access}, delegate = {delegate}, authorized = {authorized}, expires = {expires} WHERE child = {child} AND parent = {parent}")(args : _*).executeUpdate() == 0)
      Audit.SQLon(AuditAction.add, Authorize.table, Anorm.insertArgs(args))(args : _*).execute()
  }
  def remove(implicit site : Site) : Unit =
    Authorize.delete(childId, parentId)

  private[this] val _child = CachedVal[Identity, Session](Identity.get(childId)(_))
  def child(implicit db : Session) : Identity = _child
  private[this] val _parent = CachedVal[Identity, Session](Identity.get(parentId)(_))
  def parent(implicit db : Session) : Identity = _parent
}

object Authorize extends TableView("authorize") {
  private[this] val row = Anorm.rowMap(Authorize.apply _, "child", "parent", "access", "delegate", "authorized", "expires")

  private[this] def select(all : Boolean) = 
    "SELECT * FROM " + table + (if (all) "" else "_valid")

  def get(c : Int, p : Int)(implicit db : Session) : Option[Authorize] =
    SQL(select(true) + " WHERE child = {child} AND parent = {parent}").
      on('child -> c, 'parent -> p).singleOpt(row)(db.conn)

  private[models] def getParents(c : Int, all : Boolean)(implicit db : Session) =
    SQL(select(all) + " WHERE child = {child}").
      on('child -> c).list(row)(db.conn)
  private[models] def getChildren(p : Int, all : Boolean)(implicit db : Session) =
    SQL(select(all) + " WHERE parent = {parent}").
      on('parent -> p).list(row)(db.conn)

  private def delete(c : Int, p : Int)(implicit site : Site) =
    Audit.SQLon(AuditAction.remove, "authorize", "WHERE child = {child} AND parent = {parent}")('child -> c, 'parent -> p).
      execute()(site.db.conn)

  def access_check(c : Int)(implicit db : Session) : Permission.Value =
    SQL("SELECT authorize_access_check({id})").
      on('id -> c).single(scalar[Option[Permission.Value]])(db.conn).
      getOrElse(Permission.NONE)

  def delegate_check(c : Int, p : Int)(implicit db : Session) : Permission.Value =
    SQL("SELECT authorize_delegate_check({child}, {parent})").
      on('child -> c, 'parent -> p).single(scalar[Option[Permission.Value]])(db.conn).
      getOrElse(Permission.NONE)
}
