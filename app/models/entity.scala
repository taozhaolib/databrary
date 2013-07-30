package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import util._

/* Entity represents any real-world individual, group, institution, etc. */
sealed class Entity protected (val id : Entity.Id, name_ : String, orcid_ : Option[Orcid] = None) extends TableRowId[Entity] with SitePage {
  private[this] var _name = name_
  def name = _name
  private[this] var _orcid = orcid_
  def orcid = _orcid

  def change(name : String = _name, orcid : Option[Orcid] = _orcid)(implicit site : Site) : Unit = {
    if (name == _name && orcid == _orcid)
      return
    Audit.SQLon(AuditAction.change, "entity", "SET name = {name}, orcid = {orcid} WHERE id = {id}")('id -> id, 'name -> name, 'orcid -> orcid).execute()(site.db)
    _name = name
    _orcid = orcid
  }

  /* level of access user has to the site */
  private val _access = CachedVal[Permission.Value, Site.DB](Authorize.access_check(id)(_))
  def access(implicit db : Site.DB) : Permission.Value = _access

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
  final def delegatedBy(p : Entity.Id)(implicit site : Site) = Authorize.delegate_check(id, p)(site.db)

  /* List of studies accessible at (or above) the given permission level by this user */
  final def studyAccess(p : Permission.Value)(implicit site : Site) = StudyAccess.getStudies(this, p)

  /* List of comments by this individual; this does not respect access permissions on the comment targets */
  final def getComments(implicit site : Site) = Comment.getEntity(this)
}

/* Account refines Entity for individuals with registered (but not necessarily authorized) accounts on the site. */
final class Account protected (entity : Entity, val username : String, email_ : String, openid_ : Option[String], timezone_ : Option[String]) extends Entity(entity.id, entity.name, entity.orcid) with TableRowId[Account] {
  override val id = Account.asId(entity.id.unId)
  private[this] var _email = email_
  def email = _email
  private[this] var _openid = openid_
  def openid = _openid
  private[this] var _timezone = timezone_
  def timezone = _timezone

  def changeAccount(email : String = _email, openid : Option[String] = _openid, timezone : Option[String] = _timezone)(implicit site : Site) : Unit = {
    if (email == _email && openid == _openid && timezone == _timezone)
      return
    Audit.SQLon(AuditAction.change, Account.table, "SET email = {email}, openid = {openid}, timezone = {timezone} WHERE id = {id}")(
      'email -> email, 'openid -> openid, 'timezone -> timezone, 'id -> id).execute()(site.db)
    _email = email
    _openid = openid
    _timezone = timezone
  }

  override def pageName(implicit site : Site) = super.pageName + (if (site.access >= Permission.VIEW) " <" + username + ">" else "")
}

object Entity extends TableViewId[Entity]("entity") {
  private[models] override val src = "entity LEFT JOIN account USING (id)"
  private[this] def make(id : Id, name : String, orcid : Option[Orcid]) = id match {
    case NOBODY => Nobody
    case ROOT => Root
    case _ => new Entity(id, name, orcid)
  }
  private[models] val baseRow = Anorm.rowMap(make _, col("id"), col("name"), col("orcid"))
  private[models] override val * = col("*") + ", " + Account.*
  private[models] val row = (baseRow ~ Account.baseRow.?) map {
    case (e ~ None) => e
    case (e ~ Some(a)) => (Account.make(e) _).tupled(a)
  }

  def get(i : Id)(implicit site : Site) : Option[Entity] = i match {
    case NOBODY => Some(Nobody)
    case ROOT => Some(Root)
    case site.identity.id => Some(site.identity)
    case _ =>
      SQL("SELECT " + * + " FROM " + src + " WHERE id = {id}").
        on('id -> i).singleOpt(row)(site.db)
  }

  def create(name : String)(implicit site : Site) : Entity = {
    val args = Anorm.Args('name -> name)
    val id = Audit.SQLon(AuditAction.add, table, Anorm.insertArgs(args), "id")(args : _*).single(scalar[Id])(site.db)
    new Entity(id, name)
  }

  private def byName = "username = {user} OR name ILIKE {name}"
  private def byNameArgs(name : String) = Anorm.Args('user -> name, 'name -> name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%"))

  def searchForAuthorize(name : String, who : Entity.Id)(implicit db : Site.DB) =
    SQL("SELECT " + * + " FROM " + src + " WHERE " + byName + " AND id != {who} AND id NOT IN (SELECT child FROM authorize WHERE parent = {who} UNION SELECT parent FROM authorize WHERE child = {who}) LIMIT 8").
      on(Anorm.Args('who -> who) ++ byNameArgs(name) : _*).list(row)

  def searchForStudyAccess(name : String, study : Study.Id)(implicit db : Site.DB) =
    SQL("SELECT " + * + " FROM " + src + " WHERE " + byName + " AND id NOT IN (SELECT entity FROM study_access WHERE study = {study}) LIMIT 8").
      on(Anorm.Args('study -> study) ++ byNameArgs(name) : _*).list(row)

  private[models] final val NOBODY : Id = asId(-1)
  private[models] final val ROOT   : Id = asId(0)
  final val Nobody = new Entity(NOBODY, "Everybody")
  Nobody._access() = Permission.NONE // anonymous users get this level
  final val Root   = new Entity(ROOT,   "Databrary")
  Root._access() = null // the objective value is ADMIN but this should never be used
}

object Account extends TableViewId[Account]("account") {
  private[models] override val src = "entity JOIN account USING (id)"
  private[models] def make(e : Entity)(username : String, email : String, openid : Option[String], timezone : Option[String]) =
    new Account(e, username, email, openid, timezone)
  private[models] val baseRow = Anorm.rowMap(Tuple4.apply[String, String, Option[String], Option[String]] _, col("username"), col("email"), col("openid"), col("timezone"))
  private[models] override val * = col("username", "email", "openid", "timezone")
  private[models] val row = (Entity.baseRow ~ baseRow) map {
    case (e ~ a) => (make(e) _).tupled(a)
  }

  def get(i : Id)(implicit db : Site.DB) : Option[Account] = 
    SQL("SELECT " + Entity.* + " FROM " + src + " WHERE id = {id}").
      on('id -> i).singleOpt(row)
  def getUsername(u : String)(implicit db : Site.DB) : Option[Account] = 
    SQL("SELECT " + Entity.* + " FROM " + src + " WHERE username = {username}").
      on("username" -> u).singleOpt(row)
  def getOpenid(o : String, u : Option[String] = None)(implicit db : Site.DB) : Option[Account] = {
    SQL("SELECT " + Entity.* + " FROM " + src + " WHERE openid = {openid} AND coalesce(username = {username}, 't') LIMIT 1").
      on("openid" -> o, "username" -> u).singleOpt(row)
  }
}
