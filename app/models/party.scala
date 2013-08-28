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
    * Computed by [Authorize.access_check] and usually accessed through [[util.Site.access]]. */
  def access(implicit db : Site.DB) : Permission.Value = _access

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Party.view(id).url

  /** List of authorizations granted to this user.
    * @param all include inactive authorizations */
  final def authorizeParents(all : Boolean = false)(implicit db : Site.DB) = Authorize.getParents(this, all)
  /** List of authorizations granted by this user.
    * @param all include inactive authorizations */
  final def authorizeChildren(all : Boolean = false)(implicit db : Site.DB) = Authorize.getChildren(this, all)

  /** List of delegations granted to this user. */
  final def delegated(implicit site : Site) = Authorize.delegate_check(site.identity.id, id)(site.db)
  /** List of delegations granted by this user. */
  final def delegatedBy(p : Party.Id)(implicit site : Site) = Authorize.delegate_check(id, p)(site.db)

  /** List of studies accessible by this user.
    * @param p permission level to restrict to */
  final def studyAccess(p : Permission.Value)(implicit site : Site) = StudyAccess.getStudies(this, p)
}

/** Refines Party for individuals with registered (but not necessarily authorized) accounts on the site. */
final class Account protected (party : Party, email_ : String, password_ : String, openid_ : Option[String]) extends Party(party.id, party.name, party.orcid) with TableRowId[Account] {
  override val id = Account.asId(party.id.unId)
  private[this] var _email = email_
  def email = _email
  private[this] var _password = password_
  /** Crypted password, using standard unix format, currently \$2a\$-style bcrypt */
  def password = _password
  private[this] var _openid = openid_
  def openid = _openid

  /** Update the given values in the database and this object in-place. */
  def changeAccount(email : String = _email, password : String = _password, openid : Option[String] = _openid)(implicit site : Site) : Unit = {
    if (email == _email && password == _password && openid == _openid)
      return
    Audit.change(Account.table, SQLArgs('email -> email, 'password -> password, 'openid -> openid), SQLArgs('id -> id)).execute()(site.db)
    _email = email
    _password = password
    _openid = openid
  }

  /** List of comments by this individual.
    * This does not respect access permissions on the comment targets. */
  final def comments(implicit db : Site.DB) = Comment.getParty(this)(db)
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

  /** Look up a party by id. */
  def get(i : Id)(implicit site : Site) : Option[Party] = i match {
    case NOBODY => Some(Nobody)
    case ROOT => Some(Root)
    case site.identity.id => Some(site.identity)
    case _ =>
      SELECT("WHERE id = {id}").
        on('id -> i).singleOpt()(site.db)
  }

  /** Create a new party. */
  def create(name : String)(implicit site : Site) : Party = {
    val id = Audit.add(table, SQLArgs('name -> name), "id").single(scalar[Id])(site.db)
    new Party(id, name)
  }

  private def byName = "name ILIKE {name} OR email ILIKE {name}"
  private def byNameArgs(name : String) = SQLArgs('name -> name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%"))

  /** Search for parties by name for the purpose of authorization.
    * @param name string to match against name/email (case insensitive substring)
    * @param who party doing the authorization, to exclude parties already authorized
    */
  def searchForAuthorize(name : String, who : Party.Id)(implicit db : Site.DB) : Seq[Party] =
    SELECT("WHERE " + byName + " AND id != {who} AND id NOT IN (SELECT child FROM authorize WHERE parent = {who} UNION SELECT parent FROM authorize WHERE child = {who}) LIMIT 8").
      on(SQLArgs('who -> who) ++ byNameArgs(name) : _*).list()

  /** Search for parties by name for the purpose of study access.
    * @param name string to match against name/email (case insensitive substring)
    * @param study study to which to grant access, to exclude parties with access already.
    */
  def searchForStudyAccess(name : String, study : Study.Id)(implicit db : Site.DB) : Seq[Party] =
    SELECT("WHERE " + byName + " AND id NOT IN (SELECT party FROM study_access WHERE study = {study}) LIMIT 8").
      on(SQLArgs('study -> study) ++ byNameArgs(name) : _*).list()

  private[models] final val NOBODY : Id = asId(-1)
  private[models] final val ROOT   : Id = asId(0)
  /** The special party group representing everybody (including anonymous users) on the site.
    * This is also in the database, but is cached here for special handling. */
  final val Nobody = new Party(NOBODY, "Everybody")
  Nobody._access() = Permission.NONE // anonymous users get this level
  /** The special party group representing authorized users on the site.
    * This is also in the database, but is cached here for special handling. */
  final val Root   = new Party(ROOT,   "Databrary")
  Root._access() = null // the objective value is ADMIN but this should never be used
}

object Account extends TableId[Account]("account") {
  private[models] def make(e : Party)(email : String, password : Option[String], openid : Option[String]) =
    new Account(e, email, password.getOrElse(""), openid)
  private[models] val columns = Columns[
    String, Option[String], Option[String]](
    'email, 'password,      'openid)
  private[models] override val src = "party JOIN account USING (id)"
  private[models] val row = (Party.columns ~ columns) map {
    case (e ~ a) => (make(e) _).tupled(a)
  }

  /** Look up a user by id, without an active session.
    * @return None if no party or no account for given party
    */
  def get_(i : Id)(implicit db : Site.DB) : Option[Account] =
    SELECT("WHERE id = {id}").
      on('id -> i).singleOpt()
  /** Look up a user by id.
    * @return None if no party or no account for given party
    */
  def get(i : Id)(implicit site : Site) : Option[Account] =
    if (i == site.identity.id)
      site.user
    else
      get_(i)(site.db)
  /** Look up a user by email. */
  def getEmail(email : String)(implicit db : Site.DB) : Option[Account] = 
    SELECT("WHERE email = {email}").
      on('email -> email).singleOpt()
  /** Look up a user by openid.
    * @param email optionally limit results to the given email
    * @return an arbitrary account with the given openid, or the account for email if the openid matches */
  def getOpenid(openid : String, email : Option[String] = None)(implicit db : Site.DB) : Option[Account] = {
    SELECT("WHERE openid = {openid} AND coalesce(email = {email}, 't') LIMIT 1").
      on('openid -> openid, 'email -> email).singleOpt()
  }
}
