package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import macros._
import dbrary._
import site._

/** Any real-world individual, group, institution, etc.
  * Instances are generally obtained from [[Party.get]] or [[Party.create]].
  * @param delegated permission delegated by this party to the current user */
final class Party protected (val id : Party.Id, name_ : String, orcid_ : Option[Orcid], affiliation_ : Option[String]) extends TableRowId[Party] with SitePage {
  private[this] var _name = name_
  def name = _name
  private[this] var _orcid = orcid_
  def orcid = _orcid
  private[this] var _affiliation = affiliation_
  def affiliation = _affiliation

  private[models] var _account : Option[Account] = None
  def account = _account

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[String] = None, orcid : Option[Option[Orcid]] = None, affiliation : Option[Option[String]])(implicit site : Site) : Future[Boolean] = {
    Audit.change("party", SQLTerms.flatten(name.map('name -> _), orcid.map('orcid -> _), affiliation.map('affiliation -> _)), SQLTerms('id -> id))
      .execute.andThen { case scala.util.Success(true) =>
        name.foreach(_name = _)
        orcid.foreach(_orcid = _)
        affiliation.foreach(_affiliation = _)
      }
  }

  private[models] val _access : FutureVar[Permission.Value] = FutureVar[Permission.Value](Authorize.access_check(id))
  /** Level of access user has to the site.
    * Computed by [Authorize.access_check] and usually accessed through [[site.Site.access]]. */
  def access : Future[Permission.Value] = _access.apply

  /** List of authorizations granted to this user.
    * @param all include inactive authorizations */
  final def authorizeParents(all : Boolean = false) : Future[Seq[Authorize]] =
    Authorize.getParents(this, all)
  /** List of authorizations granted by this user.
    * @param all include inactive authorizations */
  final def authorizeChildren(all : Boolean = false) : Future[Seq[Authorize]] =
    Authorize.getChildren(this, all)

  /** Permission delegated by the given party to this party. */
  final def delegatedBy(p : Party.Id) : Future[Permission.Value] = Authorize.delegate_check(id, p)

  /** List of volumes to which this user has been granted at least CONTRIBUTE access, sorted by level (ADMIN first). */
  def volumeAccess(implicit site : Site) = VolumeAccess.getVolumes(this, Permission.CONTRIBUTE)

  /** List of volumes which this party is funding. */
  def funding(implicit site : Site) = VolumeFunding.getFunder(this)

  def pageName = name
  def pageParent = None
  def pageURL = controllers.routes.PartyHtml.view(id)

  def perSite(implicit site : Site) : Future[SiteParty] = SiteParty.make(this)

  def json(implicit site : Site) : JsonRecord =
    JsonRecord.flatten(id,
      Some('name -> name), 
      orcid.map('orcid -> _), 
      affiliation.map('affiliation -> _),
      account.filter(_ => site.access >= Permission.VIEW).map('email -> _.email),
      Some('avatar -> views.html.display.avatar(this))
    )
}

final class SiteParty(val party : Party, val access : Permission.Value, val delegated : Permission.Value)(implicit val site : Site) extends SiteObject {
  def ===(a : SiteParty) = party === a.party
  def ===(a : Party) = party === a

  def getPermission = Seq(delegated, Seq(site.access, Permission.DOWNLOAD).min).max

  def pageName = party.pageName
  def pageParent = party.pageParent
  def pageURL = party.pageURL
  def pageActions = Seq(
    Action("view", pageURL, Permission.VIEW),
    Action("edit", controllers.routes.PartyHtml.edit(party.id), Permission.EDIT),
    Action("authorization", controllers.routes.PartyHtml.admin(party.id), Permission.ADMIN),
    SiteAction("add volume", controllers.routes.VolumeHtml.add(Some(party.id)),
      !(party.id === Party.ROOT) && checkPermission(Permission.CONTRIBUTE) && access >= Permission.CONTRIBUTE)
  )

  def json = party.json + 
    ('permission -> getPermission)

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options,
      "parents" -> (opt => party.authorizeParents(opt.contains("all"))
        .map(JsonRecord.map(a => JsonRecord(a.parentId,
          'party -> a.parent.json,
          'access -> a.access
        )))
      ),
      "children" -> (opt => party.authorizeChildren(opt.contains("all"))
        .map(JsonRecord.map(a => JsonRecord(a.childId,
          'party -> a.child.json,
          'access -> a.access
        )))
      ),
      "volumes" -> (opt => party.volumeAccess.map(JsonArray.map(_.json - "party"))),
      "funding" -> (opt => party.funding.map(JsonArray.map(_.json - "party"))),
      "comments" -> (opt => party.account.fold[Future[Seq[Comment]]](Async(Nil))(_.comments)
        .map(JsonArray.map(c => c.json - "who" + ('volume -> c.volume.json)))
      )
    )
}

/** Refines Party for individuals with registered (but not necessarily authorized) accounts on the site. */
final class Account protected (val party : Party, email_ : String, password_ : String, openid_ : Option[String]) extends TableRow {
  def ===(a : Account) = party === a.party
  def ===(a : Party) = party === a

  party._account = Some(this)

  def id = party.id
  private[this] var _email = email_
  def email = _email
  private[this] var _password = password_
  /** Crypted password, using standard unix format, currently \$2a\$-style bcrypt */
  def password = _password
  private[this] var _openid = openid_
  def openid = _openid

  /** Update the given values in the database and this object in-place. */
  def change(email : Option[String] = None, password : Option[String] = None, openid : Option[Option[String]] = None)(implicit site : Site) : Future[Boolean] = {
    if (password != _password)
      clearTokens(cast[AuthSite](site).map(_.token))
    Audit.change(Account.table, SQLTerms.flatten(email.map('email -> _), password.map('password -> _), openid.map('openid -> _)), SQLTerms('id -> id))
      .execute.andThen { case scala.util.Success(true) =>
        email.foreach(_email = _)
        password.foreach(_password = _)
        openid.foreach(_openid = _)
      }
  }

  /** List of comments by this individual.
    * This checks permissions on the target volumes. */
  def comments(implicit site : Site) = Comment.getParty(this)

  /** Remove any issued login tokens for this user. */
  def clearTokens(except : Option[Token] = None) = AccountToken.clearAccount(id, except)
}

object Party extends TableId[Party]("party") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Option[Orcid]]("orcid")
    , SelectColumn[Option[String]]("affiliation")
    ).map { (id, name, orcid, affiliation) =>
      new Party(id, name, orcid, affiliation)
    }
  private[models] val row : Selector[Party] =
    columns.leftJoin(Account.columns, using = 'id)
    .map { case (party, acct) =>
      acct.foreach(_(party))
      party
    }

  private[models] val delegated = SelectAs[Permission.Value]("authorize_delegate_check(?, party.id)", "party_delegated")
  private[models] val access = SelectAs[Permission.Value]("authorize_access_check(party.id)", "party_access")

  /** Look up a party by id. */
  def get(i : Id)(implicit site : Site) : Future[Option[Party]] = i match {
    case NOBODY => Async(Some(Nobody))
    case ROOT => Async(Some(Root))
    case i if i === site.identity.id => Async(Some(site.identity))
    case _ => row.SELECT("WHERE id = ?").apply(i).singleOpt
  }

  /** Create a new party. */
  def create(name : String)(implicit site : Site) : Future[Party] =
    Audit.add(table, SQLTerms('name -> name), "id").single(SQLCols[Id])
      .map(new Party(_, name, None, None))

  private def byName = "(name ILIKE ? OR email ILIKE ?)"
  private def byNameArgs(name : String) =
    SQLArgs(name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%")) * 2

  /** Search for parties by name for the purpose of authorization.
    * @param name string to match against name/email (case insensitive substring)
    * @param who party doing the authorization, to exclude parties already authorized
    */
  def searchForAuthorize(name : String, who : Party) : Future[Seq[Party]] =
    row.SELECT("WHERE " + byName + " AND id != ? AND id > 0 AND id NOT IN (SELECT child FROM authorize WHERE parent = ? UNION SELECT parent FROM authorize WHERE child = ?) LIMIT 8")
      .apply(byNameArgs(name) ++ SQLArgs(who.id) * 3).list

  /** Search for parties by name for the purpose of volume access.
    * @param name string to match against name/email (case insensitive substring)
    * @param volume volume to which to grant access, to exclude parties with access already.
    */
  def searchForVolumeAccess(name : String, volume : Volume) : Future[Seq[Party]] =
    row.SELECT("WHERE " + byName + " AND id NOT IN (SELECT party FROM volume_access WHERE volume = ?) LIMIT 8").
      apply(byNameArgs(name) :+ volume.id).list

  final val NOBODY : Id = asId(-1)
  final val ROOT   : Id = asId(0)
  /** The special party group representing everybody (including anonymous users) on the site.
    * This is also in the database, but is cached here for special handling. */
  final val Nobody = new Party(NOBODY, "Everybody", None, None)
  /** The special party group representing authorized users on the site.
    * This is also in the database, but is cached here for special handling. */
  final val Root   = new Party(ROOT,   "Databrary", None, None)
}

object SiteParty {
  private[models] def make(p : Party)(implicit site : Site) : Future[SiteParty] =
    if (p === site.identity)
      Async(new SiteParty(p, site.access, if (p.id === Party.NOBODY) Permission.NONE else Permission.ADMIN))
    else
      for {
        a <- p.access
        d <- Authorize.delegate_check(site.identity.id, p.id)
      } yield (new SiteParty(p, a, d))

  def get(i : Party.Id)(implicit site : Site) : Future[Option[SiteParty]] =
    (Party.row ~ Party.access ~ Party.delegated)
      .map { case ((party, access), delegated) =>
        new SiteParty(party, access, delegated)
      }.SELECT("WHERE id = ?").apply(site.identity.id, i).singleOpt
}

object Account extends Table[Account]("account") {
  type Id = Party.Id
  private[models] val columns = Columns(
      SelectColumn[String]("email")
    , SelectColumn[Option[String]]("password")
    , SelectColumn[Option[String]]("openid")
    ).map { (email, password, openid) =>
      (party : Party) => new Account(party, email, password.getOrElse(""), openid)
    }
  private[models] val row : Selector[Account] =
    Party.columns.join(columns, using = 'id) map {
      case (party, acct) => acct(party)
    }
  private[models] val rowAccess : Selector[(Account, Permission.Value)] =
    (row ~ Party.access) map { a =>
      a._1.party._access.set(a._2)
      a
    }

  /** Look up a user by id.
    * @return None if no party or no account for given party
    */
  def get(i : Id)(implicit site : Site) : Future[Option[Account]] =
    if (i === site.identity.id) Future.successful(site.user) else // optimization
    row.SELECT("WHERE id = ?").apply(i).singleOpt
  /** Look up a user by email. */
  def getEmail(email : String) : Future[Option[Account]] =
    row.SELECT("WHERE email = ?").apply(email).singleOpt
  /** Look up a user by openid.
    * @param email optionally limit results to the given email
    * @return an arbitrary account with the given openid, or the account for email if the openid matches */
  def getOpenid(openid : String, email : Option[String] = None) : Future[Option[Account]] =
    row.SELECT("WHERE openid = ? AND coalesce(email = ?, 't') LIMIT 1").apply(openid, email).singleOpt
}
