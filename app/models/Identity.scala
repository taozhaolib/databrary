package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted
import collection.mutable.HashMap
import java.sql.Timestamp
import controllers.SiteRequest
import anorm._
import dbrary._
import util._

class Identity(entity : Entity) {
  final override def hashCode = id
  final def equals(o : Identity) = o.id == id

  protected def cache =
    IdentityCache.add(this)

  final def id = entity.id
  final def name = entity.name
  final def orcid = entity.orcid
  final def access(implicit db : Session) : Permission.Value = entity.access

  def user : Option[User] = None

  def changeEntity(name : String = name, orcid : Option[Orcid] = orcid)(implicit site : Site) =
    entity.change(name, orcid)

  final def authorizeParents(all : Boolean = false)(implicit db : Session) = Authorize.getParents(id, all)
  final def authorizeChildren(all : Boolean = false)(implicit db : Session) = Authorize.getChildren(id, all)

  final def studyAccess(p : Permission.Value)(implicit site : Site) = StudyAccess.getStudies(id, p)
}

final class User(entity : Entity, account : Account) extends Identity(entity) {
  final override def user = Some(this)

  final def username = account.username
  final def email = account.email
  final def openid = account.openid

  def changeAccount(email : String = email, openid : Option[String] = openid)(implicit site : Site) =
    account.change(email, openid)
}

private object IdentityCache extends HashMap[Int, Identity] {
  def add[I <: Identity](i : I) : I = {
    update(i.id, i)
    i
  }
  add(Identity.Nobody)
  add(Identity.Root)
}

object Identity extends TableViewId[Identity]("entity LEFT JOIN account USING (id)", _.id) {
  private[this] def apply(id : Int, name : String, orcid : Option[Orcid], username : Option[String], email : Option[String], openid : Option[String]) = id match {
    case Entity.NOBODY => Nobody
    case Entity.ROOT => Root
    case _ => {
      val e = Entity(id, name, orcid)
      username.fold(new Identity(e))(u => new User(e, new Account(id, u, email.get, openid)))
    }
  }
  private[models] val row = Anorm.rowMap(apply _, "id", "name", "orcid", "username", "email", "openid")

  def get(i : Int)(implicit db : Session) : Identity =
    IdentityCache.getOrElseUpdate(i, 
      SQL("SELECT * FROM " + table + " WHERE id = {id}").
        on('id -> Some(i)).single(row)(db.conn))

  def create(n : String)(implicit site : Site) : Identity =
    new Identity(Entity.create(n)).cache

  private def byName = "username = {user} OR name ILIKE {name}"
  private def byNameArgs(name : String) = Anorm.Args('user -> name, 'name -> name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%"))

  def searchForAuthorize(name : String, who : Identity)(implicit db : Session) =
    SQL("SELECT * FROM " + table + " WHERE " + byName + " AND id != {who} AND id NOT IN (SELECT child FROM authorize WHERE parent = {who} UNION SELECT parent FROM authorize WHERE child = {who}) LIMIT 8").
      on(Anorm.Args('who -> who) ++ byNameArgs(name) : _*).list(row)(db.conn)

  def searchForStudyAccess(name : String, study : Study)(implicit db : Session) =
    SQL("SELECT * FROM " + table + " WHERE " + byName + " AND id NOT IN (SELECT entity FROM study_access WHERE study = {study}) LIMIT 8").
      on(Anorm.Args('study -> study) ++ byNameArgs(name) : _*).list(row)(db.conn)

  final val Nobody = new Identity(Entity.Nobody)
  final val Root   = new Identity(Entity.Root)
}

object User extends TableViewId[User]("entity JOIN account USING (id)", _.id) {
  private[this] def apply(id : Int, name : String, orcid : Option[Orcid], username : String, email : String, openid : Option[String]) =
    new User(new Entity(id, name, orcid), new Account(id, username, email, openid))
  private[models] val row = Anorm.rowMap(apply _, "id", "name", "orcid", "username", "email", "openid")

  def get(i : Int)(implicit db : Session) : Option[User] = 
    Identity.get(i).user
  def getUsername(u : String)(implicit db : Session) : Option[User] = 
    SQL("SELECT * FROM " + table + " WHERE username = {username}").
      on("username" -> u).singleOpt(row)(db.conn)/*.map(_.cache)*/
  def getOpenid(o : String, u : Option[String] = None)(implicit db : Session) : Option[User] = {
    SQL("SELECT * FROM " + table + " WHERE openid = {openid} AND coalesce(username = {username}, 't') LIMIT 1").
      on("openid" -> o, "username" -> u).singleOpt(row)(db.conn)/*.map(_.cache)*/
  }
}
