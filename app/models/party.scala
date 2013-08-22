package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import util._

/** Any real-world individual, group, institution, etc.
  * Instances are generally obtained from [[Party.get]] or [[Party.create]]. */
sealed class Party protected (val id : Party.Id, name_ : String, orcid_ : Option[Orcid] = None) extends TableRowId[Party] with SitePage {
  private[this] var _name = name_
  def name = _name
  private[this] var _orcid = orcid_
  def orcid = _orcid

  /** Update the given values in the database and this object in-place. */
  def change(name : String = _name, orcid : Option[Orcid] = _orcid)(implicit site : Site) : Unit = {
    if (name == _name && orcid == _orcid)
      return
    Audit.change("party", SQLArgs('name -> name, 'orcid -> orcid), SQLArgs('id -> id)).execute()(site.db)
    _name = name
    _orcid = orcid
  }

  private val _access = CachedVal[Permission.Value, Site.DB](Authorize.access_check(id)(_))
  /** Level of access user has to the site.
    * Usually accessed through [[util.Site.access]]. */
  def access(implicit db : Site.DB) : Permission.Value = _access

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Party.view(id).url

  /* List of authorizations granted to this user, including inactive ones if all */
  final def authorizeParents(all : Boolean = false)(implicit db : Site.DB) = Authorize.getParents(this, all)
  /* List of authorizations granted by this user */
  final def authorizeChildren(all : Boolean = false)(implicit db : Site.DB) = Authorize.getChildren(this, all)

  /* List of delegations granted to this user */
  final def delegated(implicit site : Site) = Authorize.delegate_check(site.identity.id, id)(site.db)
  /* List of delegations granted by this user */
  final def delegatedBy(p : Party.Id)(implicit site : Site) = Authorize.delegate_check(id, p)(site.db)

  /* List of studies accessible at (or above) the given permission level by this user */
  final def studyAccess(p : Permission.Value)(implicit site : Site) = StudyAccess.getStudies(this, p)

  /* List of comments by this individual; this does not respect access permissions on the comment targets */
  final def comments(implicit db : Site.DB) = Comment.getParty(this)(db)
}

/* Account refines Party for individuals with registered (but not necessarily authorized) accounts on the site. */
final class Account protected (party : Party, val username : String, email_ : String, openid_ : Option[String]) extends Party(party.id, party.name, party.orcid) with TableRowId[Account] {
  override val id = Account.asId(party.id.unId)
  private[this] var _email = email_
  def email = _email
  private[this] var _openid = openid_
  def openid = _openid

  def changeAccount(email : String = _email, openid : Option[String] = _openid)(implicit site : Site) : Unit = {
    if (email == _email && openid == _openid)
      return
    Audit.change(Account.table, SQLArgs('email -> email, 'openid -> openid), SQLArgs('id -> id)).execute()(site.db)
    _email = email
    _openid = openid
  }

  override def pageName(implicit site : Site) = super.pageName + (if (site.access >= Permission.VIEW) " <" + username + ">" else "")
}

object Party extends TableId[Party]("party") {
  private[this] def make(id : Id, name : String, orcid : Option[Orcid]) = id match {
    case NOBODY => Nobody
    case ROOT => Root
    case _ => new Party(id, name, orcid)
  }
  private[models] val columns = Columns[
    Id,  String, Option[Orcid]](
    'id, 'name,  'orcid).
    map(make _)
  private[models] override val src = "party LEFT JOIN account USING (id)"
  private[models] val row = (columns ~ Account.columns.?) map {
    case (e ~ None) => e
    case (e ~ Some(a)) => (Account.make(e) _).tupled(a)
  }

  def get(i : Id)(implicit site : Site) : Option[Party] = i match {
    case NOBODY => Some(Nobody)
    case ROOT => Some(Root)
    case site.identity.id => Some(site.identity)
    case _ =>
      SELECT("WHERE id = {id}").
        on('id -> i).singleOpt()(site.db)
  }

  def create(name : String)(implicit site : Site) : Party = {
    val id = Audit.add(table, SQLArgs('name -> name), "id").single(scalar[Id])(site.db)
    new Party(id, name)
  }

  private def byName = "username = {user} OR name ILIKE {name}"
  private def byNameArgs(name : String) = SQLArgs('user -> name, 'name -> name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%"))

  def searchForAuthorize(name : String, who : Party.Id)(implicit db : Site.DB) : Seq[Party] =
    SELECT("WHERE " + byName + " AND id != {who} AND id NOT IN (SELECT child FROM authorize WHERE parent = {who} UNION SELECT parent FROM authorize WHERE child = {who}) LIMIT 8").
      on(SQLArgs('who -> who) ++ byNameArgs(name) : _*).list()

  def searchForStudyAccess(name : String, study : Study.Id)(implicit db : Site.DB) : Seq[Party] =
    SELECT("WHERE " + byName + " AND id NOT IN (SELECT party FROM study_access WHERE study = {study}) LIMIT 8").
      on(SQLArgs('study -> study) ++ byNameArgs(name) : _*).list()

  private[models] final val NOBODY : Id = asId(-1)
  private[models] final val ROOT   : Id = asId(0)
  final val Nobody = new Party(NOBODY, "Everybody")
  Nobody._access() = Permission.NONE // anonymous users get this level
  final val Root   = new Party(ROOT,   "Databrary")
  Root._access() = null // the objective value is ADMIN but this should never be used
}

object Account extends TableId[Account]("account") {
  private[models] def make(e : Party)(username : String, email : String, openid : Option[String]) =
    new Account(e, username, email, openid)
  private[models] val columns = Columns[
    String,    String,  Option[String]](
    'username, 'email,  'openid)
  private[models] override val src = "party JOIN account USING (id)"
  private[models] val row = (Party.columns ~ columns) map {
    case (e ~ a) => (make(e) _).tupled(a)
  }

  def get(i : Id)(implicit db : Site.DB) : Option[Account] = 
    SELECT("WHERE id = {id}").
      on('id -> i).singleOpt()
  def getUsername(u : String)(implicit db : Site.DB) : Option[Account] = 
    SELECT("WHERE username = {username}").
      on('username -> u).singleOpt()
  def getOpenid(o : String, u : Option[String] = None)(implicit db : Site.DB) : Option[Account] = {
    SELECT("WHERE openid = {openid} AND coalesce(username = {username}, 't') LIMIT 1").
      on('openid -> o, 'username -> u).singleOpt()
  }
}
