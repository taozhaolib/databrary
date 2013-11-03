package models

import dbrary._
import site._

/** Any real-world individual, group, institution, etc.
  * Instances are generally obtained from [[Party.get]] or [[Party.create]]. */
sealed class Party protected (val id : Party.Id, name_ : String, orcid_ : Option[Orcid] = None) extends TableRowId[Party] with SitePage with HasPermission {
  private[this] var _name = name_
  def name = _name
  private[this] var _orcid = orcid_
  def orcid = _orcid

  /** Update the given values in the database and this object in-place. */
  def change(name : String = _name, orcid : Option[Orcid] = _orcid)(implicit site : Site) : Unit = {
    if (name == _name && orcid == _orcid)
      return
    Audit.change("party", SQLTerms('name -> name, 'orcid -> orcid), SQLArgs('id -> id)).run()
    _name = name
    _orcid = orcid
  }

  /** Level of access user has to the site.
    * Computed by [Authorize.access_check] and usually accessed through [[site.Site.access]]. */
  lazy val access : Future[Permission.Value] = Authorize.access_check(id)

  private[this] val _authorizeParents = CachedVal[Seq[Authorize], Site.DB](Authorize.getParents(this)(_))
  /** List of authorizations granted to this user. Cached for !all.
    * @param all include inactive authorizations */
  final def authorizeParents(all : Boolean = false)(implicit db : Site.DB) : Seq[Authorize] =
    if (all) Authorize.getParents(this, all)
    else _authorizeParents
  /** List of authorizations granted by this user.
    * @param all include inactive authorizations */
  final def authorizeChildren(all : Boolean = false)(implicit db : Site.DB) : Seq[Authorize] = Authorize.getChildren(this, all)

  private[this] val _delegated = CachedVal[Permission.Value, Site](site => Authorize.delegate_check(site.identity.id, id)(site.db))
  /** Permission delegated by this party to the current user. */
  final def delegated(implicit site : Site) : Permission.Value = _delegated
  /** Permission delegated by the given party to this party. */
  final def delegatedBy(p : Party.Id)(implicit site : Site) : Permission.Value = Authorize.delegate_check(id, p)

  /** List of volumes to which this user has been granted access.
    * @param p permission level to restrict to
    * @return VolumeAccess sorted by level (ADMIN first). */
  final def volumeAccess(p : Permission.Value)(implicit site : Site) = VolumeAccess.getVolumes(this, p)

  /** List of volumes which this party is funding. */
  final def funding(implicit site : Site) : Seq[VolumeFunding] = VolumeFunding.getFunder(this)

  def getPermission(implicit site : Site) = delegated

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = None
  def pageURL(implicit site : Site) = controllers.routes.Party.view(id)
  def pageActions(implicit site : Site) = Seq(
    Action("view", controllers.routes.Party.view(id), Permission.VIEW),
    Action("edit", controllers.routes.Party.edit(id), Permission.EDIT),
    Action("authorization", controllers.routes.Party.admin(id), Permission.ADMIN),
    SiteAction("add volume", controllers.routes.Volume.create(Some(id)), checkPermission(Permission.CONTRIBUTE) && access >= Permission.CONTRIBUTE)
  )
}

/** Refines Party for individuals with registered (but not necessarily authorized) accounts on the site. */
final class Account protected (party : Party, email_ : String, password_ : String, openid_ : Option[String], access_ : Permission.Value) extends Party(party.id, party.name, party.orcid) with TableRowId[Account] {
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
    Audit.change(Account.table, SQLArgs('email -> email, 'password -> password, 'openid -> openid), SQLArgs('id -> id)).execute()
    if (password != _password)
      clearTokens
    _email = email
    _password = password
    _openid = openid
  }

  override def access(implicit db : Site.DB) = access_

  /** List of comments by this individual.
    * This checks permissions on the target volumes. */
  def comments(implicit site : Site) = Comment.getParty(this)

  /** Remove any issued login tokens for this user. */
  def clearTokens(implicit db : Site.DB) = LoginToken.clearAccount(id)
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
  private[models] val row = columns.leftJoin(Account.columns, using = 'id) map {
    case (e ~ None) => e
    case (e ~ Some(a)) => (Account.make(e) _).tupled(a)
  }

  /** Look up a party by id. */
  def get(i : Id)(implicit site : Site) : Option[Party] = i match {
    case NOBODY => Some(Nobody)
    case ROOT => Some(Root)
    case site.identity.id => Some(site.identity)
    case _ =>
      row.SQL("WHERE id = {id}").
        on('id -> i).singleOpt()
  }

  /** Create a new party. */
  def create(name : String)(implicit site : Site) : Party = {
    val id = Audit.add(table, SQLTerms('name -> name), "id").single(scalar[Id])
    new Party(id, name)
  }

  private def byName = "name ILIKE {name} OR email ILIKE {name}"
  private def byNameArgs(name : String) = SQLArgs('name -> name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%"))

  /** Search for parties by name for the purpose of authorization.
    * @param name string to match against name/email (case insensitive substring)
    * @param who party doing the authorization, to exclude parties already authorized
    */
  def searchForAuthorize(name : String, who : Party.Id)(implicit db : Site.DB) : Seq[Party] =
    row.SQL("WHERE " + byName + " AND id != {who} AND id > 0 AND id NOT IN (SELECT child FROM authorize WHERE parent = {who} UNION SELECT parent FROM authorize WHERE child = {who}) LIMIT 8").
      on(SQLArgs('who -> who) ++ byNameArgs(name) : _*).list()

  /** Search for parties by name for the purpose of volume access.
    * @param name string to match against name/email (case insensitive substring)
    * @param volume volume to which to grant access, to exclude parties with access already.
    */
  def searchForVolumeAccess(name : String, volume : Volume.Id)(implicit db : Site.DB) : Seq[Party] =
    row.SQL("WHERE " + byName + " AND id NOT IN (SELECT party FROM volume_access WHERE volume = {volume}) LIMIT 8").
      on(SQLArgs('volume -> volume) ++ byNameArgs(name) : _*).list()

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
  private[models] def make(e : Party)(email : String, password : Option[String], openid : Option[String], access : Option[Permission.Value]) =
    new Account(e, email, password.getOrElse(""), openid, access.getOrElse(Permission.NONE))
  private[models] val columns = Columns[
    String, Option[String], Option[String], Option[Permission.Value]](
    'email, 'password,      'openid,        SelectAs("authorize_access_check(party.id)", "party_access"))
  private[models] val row = Party.columns.join(columns, using = 'id) map {
    case (e ~ a) => (make(e) _).tupled(a)
  }

  /** Look up a user by id, without an active session.
    * @return None if no party or no account for given party
    */
  def get_(i : Int) : Future[Option[Account]] =
    row.SELECT("WHERE id = ?")(SQLArgs(i)).singleOpt
  /** Look up a user by id.
    * @return None if no party or no account for given party
    */
  def get(i : Id)(implicit site : Site) : Future[Option[Account]] =
    if (i == site.identity.id) Future.successful(site.user)
    else row.SELECT("WHERE id = ?")(SQLArgs(i)).singleOpt
  /** Look up a user by email. */
  def getEmail(email : String) : Future[Option[Account]] = 
    row.SELECT("WHERE email = ?")(SQLArgs(email)).singleOpt
  /** Look up a user by openid.
    * @param email optionally limit results to the given email
    * @return an arbitrary account with the given openid, or the account for email if the openid matches */
  def getOpenid(openid : String, email : Option[String] = None) : Future[Option[Account]] =
    row.SELECT("WHERE openid = ? AND coalesce(email = ?, 't') LIMIT 1")(SQLArgs(openid, email)).singleOpt
}
