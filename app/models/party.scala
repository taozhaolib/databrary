package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** Any real-world individual, group, institution, etc.
  * Instances are generally obtained from [[Party.get]] or [[Party.create]].
  * @param delegated permission delegated by this party to the current user */
sealed class Party protected (val id : Party.Id, name_ : String, orcid_ : Option[Orcid], val delegated : Permission.Value)(implicit val site : Site) extends TableRowId[Party] with SitePage with HasPermission {
  private[this] var _name = name_
  def name = _name
  private[this] var _orcid = orcid_
  def orcid = _orcid

  /** Update the given values in the database and this object in-place. */
  def change(name : String = _name, orcid : Option[Orcid] = _orcid) : Future[Boolean] = {
    if (name == _name && orcid == _orcid)
      return Async(true)
    Audit.change("party", SQLTerms('name -> name, 'orcid -> orcid), SQLTerms('id -> id))
      .execute.andThen { case scala.util.Success(true) =>
        _name = name
        _orcid = orcid
      }
  }

  /** Level of access user has to the site.
    * Computed by [Authorize.access_check] and usually accessed through [[site.Site.access]]. */
  lazy val access : Future[Permission.Value] = Authorize.access_check(id)

  private[this] lazy val _authorizeParents : Future[Seq[Authorize]] = Authorize.getParents(this)
  /** List of authorizations granted to this user. Cached for !all.
    * @param all include inactive authorizations */
  final def authorizeParents(all : Boolean = false) : Future[Seq[Authorize]] =
    if (all) Authorize.getParents(this, all)
    else _authorizeParents
  /** List of authorizations granted by this user.
    * @param all include inactive authorizations */
  final def authorizeChildren(all : Boolean = false) : Future[Seq[Authorize]] =
    Authorize.getChildren(this, all)

  /** Permission delegated by the given party to this party. */
  final def delegatedBy(p : Party.Id) : Future[Permission.Value] = Authorize.delegate_check(id, p)

  /** List of volumes to which this user has been granted at least CONTRIBUTE access, sorted by level (ADMIN first). */
  final lazy val volumeAccess = VolumeAccess.getVolumes(this, Permission.CONTRIBUTE)

  /** List of volumes which this party is funding. */
  final lazy val funding : Future[Seq[VolumeFunding]] = VolumeFunding.getFunder(this)

  def getPermission = delegated

  def pageName = name
  def pageParent = None
  def pageURL = controllers.routes.Party.view(id)
  def pageActions = Seq(
    Action("view", controllers.routes.Party.view(id), Permission.VIEW),
    Action("edit", controllers.routes.Party.edit(id), Permission.EDIT),
    Action("authorization", controllers.routes.Party.admin(id), Permission.ADMIN),
    SiteAction("add volume", controllers.routes.Volume.create(Some(id)),
      !id.equals(Party.ROOT) && checkPermission(Permission.CONTRIBUTE) && Async.get(access) >= Permission.CONTRIBUTE)
  )
}

/** Refines Party for individuals with registered (but not necessarily authorized) accounts on the site. */
final class Account protected (party : Party, email_ : String, password_ : String, openid_ : Option[String])(implicit site : Site) extends Party(party.id, party.name, party.orcid, party.delegated)(site) with TableRowId[Account] {
  override val id = Account.asId(party.id.unId)
  private[this] var _email = email_
  def email = _email
  private[this] var _password = password_
  /** Crypted password, using standard unix format, currently \$2a\$-style bcrypt */
  def password = _password
  private[this] var _openid = openid_
  def openid = _openid

  /** Update the given values in the database and this object in-place. */
  def changeAccount(email : String = _email, password : String = _password, openid : Option[String] = _openid) : Future[Boolean] = {
    if (email == _email && password == _password && openid == _openid)
      return Async(true)
    if (password != _password)
      clearTokens
    Audit.change(Account.table, SQLTerms('email -> email, 'password -> password, 'openid -> openid), SQLTerms('id -> id))
      .execute.andThen { case scala.util.Success(true) =>
        _email = email
        _password = password
        _openid = openid
      }
  }

  /** List of comments by this individual.
    * This checks permissions on the target volumes. */
  def comments = Comment.getParty(this)

  /** Remove any issued login tokens for this user. */
  def clearTokens = LoginToken.clearAccount(id)
}

object Party extends TableId[Party]("party") {
  private[this] def make(id : Id, name : String, orcid : Option[Orcid], delegated : Permission.Value)(implicit site : Site) =
    new Party(id, name, orcid, delegated)
  private[models] val baseColumns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Option[Orcid]]("orcid")
    )
  private[models] def columns(implicit site : Site) =
    (baseColumns ~+ SelectAs[Permission.Value]("authorize_delegate_check(?, party.id)", "party_delegated"))
    .map(make _).pushArgs(SQLArgs(site.identity.id))
  private[models] def row(implicit site : Site) : Selector[Party] =
    columns.leftJoin(Account.columns, using = 'id) map {
      case (e, None) => e
      case (e, Some(a)) => (Account.make(e) _).tupled(a)
    }

  /** Look up a party by id. */
  def get(i : Id)(implicit site : Site) : Future[Option[Party]] = i match {
    //case NOBODY => Async(Some(Nobody))
    //case ROOT => Async(Some(Root))
    case site.identity.id => Async(Some(site.identity))
    case _ => row.SELECT("WHERE id = ?").apply(i).singleOpt
  }

  /** Create a new party. */
  def create(name : String)(implicit site : Site) : Future[Party] =
    Audit.add(table, SQLTerms('name -> name), "id").single(SQLCols[Id])
      .map(new Party(_, name, None, Permission.NONE))

  private def byName = "name ILIKE ? OR email ILIKE ?"
  private def byNameArgs(name : String) =
    SQLArgs(name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%")) * 2

  /** Search for parties by name for the purpose of authorization.
    * @param name string to match against name/email (case insensitive substring)
    * @param who party doing the authorization, to exclude parties already authorized
    */
  def searchForAuthorize(name : String, who : Party) : Future[Seq[Party]] =
    row(who.site).SELECT("WHERE " + byName + " AND id != ? AND id > 0 AND id NOT IN (SELECT child FROM authorize WHERE parent = ? UNION SELECT parent FROM authorize WHERE child = ?) LIMIT 8")
      .apply(byNameArgs(name) ++ SQLArgs(who.id) * 3).list

  /** Search for parties by name for the purpose of volume access.
    * @param name string to match against name/email (case insensitive substring)
    * @param volume volume to which to grant access, to exclude parties with access already.
    */
  def searchForVolumeAccess(name : String, volume : Volume) : Future[Seq[Party]] =
    row(volume.site).SELECT("WHERE " + byName + " AND id NOT IN (SELECT party FROM volume_access WHERE volume = ?) LIMIT 8").
      apply(byNameArgs(name) :+ volume.id).list

  final val NOBODY : Id = asId(-1)
  final val ROOT   : Id = asId(0)
  /** The special party group representing everybody (including anonymous users) on the site.
    * This is also in the database, but is cached here for special handling. */
  final val Nobody = new Party(NOBODY, "Everybody", None, Permission.NONE)(Site.Anon)
  /** The special party group representing authorized users on the site.
    * This is also in the database, but is cached here for special handling. */
  // final val Root   = new Party(ROOT,   "Databrary")
}

object Account extends TableId[Account]("account") {
  private[models] def make(e : Party)(email : String, password : Option[String], openid : Option[String])(implicit site : Site) =
    new Account(e, email, password.getOrElse(""), openid)(site)
  private[models] val columns = Columns(
      SelectColumn[String]("email")
    , SelectColumn[Option[String]]("password")
    , SelectColumn[Option[String]]("openid")
    )
  private[models] def row(implicit site : Site) : Selector[Account] =
    Party.columns.join(columns, using = 'id) map {
      case (e, a) => (make(e) _).tupled(a)
    }
  private val access = Columns(SelectAs[Permission.Value]("authorize_access_check(party.id)", "party_access"))

  /** Look up a user by id, without an active session.
    * @return None if no party or no account for given party
    */
  def login(i : Int) : Future[Option[(Account, Permission.Value)]] =
    (Party.baseColumns.join(columns, using = 'id) ~ access)
      .map {
        case (((id, name, orcid), (email, password, openid)), access) =>
          new Account(new Party)
      .SELECT("WHERE id = ?").apply(i).singleOpt
      case Some((p, a)) => 
  /** Look up a user by id.
    * @return None if no party or no account for given party
    */
  def get(i : Id)(implicit site : Site) : Future[Option[Account]] =
    if (i == site.identity.id) Future.successful(site.user) else // optimization
    row.SELECT("WHERE id = ?").apply(i).singleOpt
  /** Look up a user by email. */
  def getEmail(email : String)(implicit site : Site) : Future[Option[Account]] = 
    row.SELECT("WHERE email = ?").apply(email).singleOpt
  /** Look up a user by openid.
    * @param email optionally limit results to the given email
    * @return an arbitrary account with the given openid, or the account for email if the openid matches */
  def getOpenid(openid : String, email : Option[String] = None)(implicit site : Site) : Future[Option[Account]] =
    row.SELECT("WHERE openid = ? AND coalesce(email = ?, 't') LIMIT 1").apply(openid, email).singleOpt
}
