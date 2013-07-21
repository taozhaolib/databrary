package models

import collection.mutable.HashMap
import anorm._
import dbrary._
import util._

/* Generic representation of user identity, which may be authenticated (as User) or anonymous */
sealed class Identity(private val entity : Entity) extends TableRowId[Entity] with SitePage {
  protected def cache =
    IdentityCache.add(this)

  final val id = entity.id
  final def name = entity.name
  final def orcid = entity.orcid
  /* level of access user has to the site */
  final def access(implicit db : Site.DB) : Permission.Value = entity.access

  /* shorthand for asInstanceOf[User] */
  def user : Option[User] = None

  def changeEntity(name : String = name, orcid : Option[Orcid] = orcid)(implicit site : Site) =
    entity.change(name, orcid)

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Entity.view(id).url

  /* List of authorizations granted to this user, including inactive ones if all */
  final def authorizeParents(all : Boolean = false)(implicit db : Site.DB) = Authorize.getParents(this, all)
  /* List of authorizations granted by this user */
  final def authorizeChildren(all : Boolean = false)(implicit db : Site.DB) = Authorize.getChildren(this, all)

  /* List of delegations granted to this user */
  final def delegated(implicit site : Site) = Authorize.delegate_check(site.identity.id, id)(site.db)
  /* List of delegations granted by this user */
  final def delegatedBy(p : Identity.Id)(implicit site : Site) = Authorize.delegate_check(id, p)(site.db)

  /* List of studies accessible at (or above) the given permission level by this user */
  final def studyAccess(p : Permission.Value)(implicit site : Site) = StudyAccess.getStudies(this, p)

  /* List of comments by this individual; this does not respect access permissions on the comment targets */
  final def getComments(implicit site : Site) = Comment.getEntity(this)
}

final class User(entity : Entity, account : Account) extends Identity(entity) {
  final override def user = Some(this)

  final def username = account.username
  final def email = account.email
  final def openid = account.openid

  def changeAccount(email : String = email, openid : Option[String] = openid)(implicit site : Site) =
    account.change(email, openid)

  override def pageName(implicit site : Site) = super.pageName + (if (site.access >= Permission.VIEW) " <" + username + ">" else "")
}

/* This is of questionable utility, but is primarily a proof of concept */
private object IdentityCache extends HashMap[Int, Identity] {
  def add[I <: Identity](i : I) : I = {
    update(i.id.unId, i)
    i
  }
  add(Identity.Nobody)
  add(Identity.Root)
}

object Identity extends TableView[Identity]("entity LEFT JOIN account USING (id)") with HasId[Entity] {
  private[models] override val * = Entity.* + ", " + Account.*
  private[models] val row = (Entity.row ~ Account.row.?) map {
    case (e ~ None) => new Identity(e)
    case (e ~ Some(a)) => new User(e, a)
  }

  def get(i : Id)(implicit db : Site.DB) : Option[Identity] =
    Option(IdentityCache.getOrElseUpdate(i.unId, 
      SQL("SELECT * FROM " + src + " WHERE id = {id}").
        on('id -> i).singleOpt(row).orNull))

  def create(n : String)(implicit site : Site) : Identity =
    new Identity(Entity.create(n)).cache

  private def byName = "username = {user} OR name ILIKE {name}"
  private def byNameArgs(name : String) = Anorm.Args('user -> name, 'name -> name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%"))

  def searchForAuthorize(name : String, who : Identity.Id)(implicit db : Site.DB) =
    SQL("SELECT * FROM " + src + " WHERE " + byName + " AND id != {who} AND id NOT IN (SELECT child FROM authorize WHERE parent = {who} UNION SELECT parent FROM authorize WHERE child = {who}) LIMIT 8").
      on(Anorm.Args('who -> who) ++ byNameArgs(name) : _*).list(row)

  def searchForStudyAccess(name : String, study : Study.Id)(implicit db : Site.DB) =
    SQL("SELECT * FROM " + src + " WHERE " + byName + " AND id NOT IN (SELECT entity FROM study_access WHERE study = {study}) LIMIT 8").
      on(Anorm.Args('study -> study) ++ byNameArgs(name) : _*).list(row)

  final val Nobody = new Identity(Entity.Nobody)
  final val Root   = new Identity(Entity.Root)
}

object User extends TableView[User]("entity JOIN account USING (id)") with HasId[Entity] {
  private[models] override val * = Entity.* + ", " + Account.*
  private[models] val row = (Entity.row ~ Account.row) map {
    case (e ~ a) => new User(e, a)
  }

  def get(i : Id)(implicit db : Site.DB) : Option[User] = 
    Identity.get(i).flatMap(_.user)
  def getUsername(u : String)(implicit db : Site.DB) : Option[User] = 
    SQL("SELECT * FROM " + src + " WHERE username = {username}").
      on("username" -> u).singleOpt(row)/*.map(_.cache)*/
  def getOpenid(o : String, u : Option[String] = None)(implicit db : Site.DB) : Option[User] = {
    SQL("SELECT * FROM " + src + " WHERE openid = {openid} AND coalesce(username = {username}, 't') LIMIT 1").
      on("openid" -> o, "username" -> u).singleOpt(row)/*.map(_.cache)*/
  }
}
