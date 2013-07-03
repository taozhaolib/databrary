package models

import collection.mutable.HashMap
import anorm._
import dbrary._
import util._

class Identity(private val entity : Entity) extends TableRowId(entity.id.unId) {
  protected def cache =
    IdentityCache.add(this)

  final def id = entity.id
  final def name = entity.name
  final def orcid = entity.orcid
  final def access(implicit db : Site.DB) : Permission.Value = entity.access

  def user : Option[User] = None

  def changeEntity(name : String = name, orcid : Option[Orcid] = orcid)(implicit site : Site) =
    entity.change(name, orcid)

  final def authorizeParents(all : Boolean = false)(implicit db : Site.DB) = Authorize.getParents(id, all)
  final def authorizeChildren(all : Boolean = false)(implicit db : Site.DB) = Authorize.getChildren(id, all)

  final def delegated(implicit site : Site) = Authorize.delegate_check(site.identity.id, id)(site.db)
  final def delegatedBy(p : Identity.Id)(implicit site : Site) = Authorize.delegate_check(id, p)(site.db)
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
    update(i.id.unId, i)
    i
  }
  add(Identity.Nobody)
  add(Identity.Root)
}

object Identity extends TableView[Identity]("entity LEFT JOIN account USING (id)") {
  type Id = Entity.Id
  def asId(i : Int) : Id = Entity.asId(i)

  private[models] val row = (Entity.row ~ Account.row.?).map({
    case (e ~ None) => new Identity(e)
    case (e ~ Some(a)) => new User(e, a)
  })

  def get(i : Id)(implicit db : Site.DB) : Identity =
    IdentityCache.getOrElseUpdate(i.unId, 
      SQL("SELECT * FROM " + table + " WHERE id = {id}").
        on('id -> i).single(row))

  def create(n : String)(implicit site : Site) : Identity =
    new Identity(Entity.create(n)).cache

  private def byName = "username = {user} OR name ILIKE {name}"
  private def byNameArgs(name : String) = Anorm.Args('user -> name, 'name -> name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%"))

  def searchForAuthorize(name : String, who : Identity.Id)(implicit db : Site.DB) =
    SQL("SELECT * FROM " + table + " WHERE " + byName + " AND id != {who} AND id NOT IN (SELECT child FROM authorize WHERE parent = {who} UNION SELECT parent FROM authorize WHERE child = {who}) LIMIT 8").
      on(Anorm.Args('who -> who) ++ byNameArgs(name) : _*).list(row)

  def searchForStudyAccess(name : String, study : Study.Id)(implicit db : Site.DB) =
    SQL("SELECT * FROM " + table + " WHERE " + byName + " AND id NOT IN (SELECT entity FROM study_access WHERE study = {study}) LIMIT 8").
      on(Anorm.Args('study -> study) ++ byNameArgs(name) : _*).list(row)

  final val Nobody = new Identity(Entity.Nobody)
  final val Root   = new Identity(Entity.Root)
}

object User extends TableView[User]("entity JOIN account USING (id)") {
  type Id = Identity.Id
  def asId(i : Int) : Id = Identity.asId(i)

  private[models] val row = (Entity.row ~ Account.row).map({
    case (e ~ a) => new User(e, a)
  })

  def get(i : Id)(implicit db : Site.DB) : Option[User] = 
    Identity.get(i).user
  def getUsername(u : String)(implicit db : Site.DB) : Option[User] = 
    SQL("SELECT * FROM " + table + " WHERE username = {username}").
      on("username" -> u).singleOpt(row)/*.map(_.cache)*/
  def getOpenid(o : String, u : Option[String] = None)(implicit db : Site.DB) : Option[User] = {
    SQL("SELECT * FROM " + table + " WHERE openid = {openid} AND coalesce(username = {username}, 't') LIMIT 1").
      on("openid" -> o, "username" -> u).singleOpt(row)/*.map(_.cache)*/
  }
}
