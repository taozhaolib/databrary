package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{Json,JsNull}
import java.net.URL
import macros._
import dbrary._
import site._

/** Any real-world individual, group, institution, etc.
  * Instances are generally obtained from [[Party.get]] or [[Party.create]].
  * @param delegated permission delegated by this party to the current user */
final class Party protected (val id : Party.Id, private[this] var name_ : String, private[this] var orcid_ : Option[Orcid], private[this] var affiliation_ : Option[String], private[this] var url_ : Option[URL] = None)
  extends TableRowId[Party] with SitePage {
  def name = name_
  def orcid = orcid_
  def affiliation = affiliation_
  def url = url_

  private[models] var account_ : Option[Account] = None
  def account = account_

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[String] = None, orcid : Option[Option[Orcid]] = None, affiliation : Option[Option[String]], url : Option[Option[URL]])(implicit site : Site) : Future[Boolean] =
    Audit.change("party", SQLTerms.flatten(
      name.map('name -> _),
      orcid.map('orcid -> _),
      affiliation.map('affiliation -> _),
      url.map('url -> _)),
      SQLTerms('id -> id))
    .execute.andThen { case scala.util.Success(true) =>
      name.foreach(name_ = _)
      orcid.foreach(orcid_ = _)
      affiliation.foreach(affiliation_ = _)
      url.foreach(url_ = _)
    }

  /** List of authorizations granted to this user.
    * @param all include inactive authorizations */
  def authorizeParents(all : Boolean = false) : Future[Seq[Authorize]] =
    Authorize.getParents(this, all)
  /** List of authorizations granted by this user.
    * @param all include inactive authorizations */
  def authorizeChildren(all : Boolean = false) : Future[Seq[Authorize]] =
    Authorize.getChildren(this, all)

  private[models] val _access : FutureVar[Access] =
    FutureVar[Access](Authorization.get(this))
  /** Level of access user has to the site.
    * Usually accessed through [[site.Site.access]] or [[SiteParty]]. */
  def access : Future[Access] = _access.apply

  def pageName = name
  def pageParent = None
  def pageURL = controllers.routes.PartyHtml.view(id, None)

  def perSite(implicit site : Site) : Future[SiteParty] = SiteParty.get(this)
  /** Email, if accessible, for convenience. */
  def email(implicit site : Site) : Option[String] =
    account.filter(_ => site.access.site >= Permission.SHARED).map(_.email)

  def json(implicit site : Site) : JsonRecord =
    JsonRecord.flatten(id
    , Some('name -> name)
    , orcid.map(('orcid, _))
    , affiliation.map(('affiliation, _))
    , email.map(('email, _))
    , if (account.isEmpty) Some('institution -> true) else None
    , url.map(('url, _))
    )
}

final class SiteParty(access : Access)(implicit val site : Site)
  extends SiteObject {
  assert(access.identity === site.identity)
  def party = access.target
  def id = party.id
  def ===(a : SiteParty) = party === a.party
  def ===(a : Party) = party === a

  def permission = max(access.permission, min(site.access.site, Permission.READ))

  /** List of volumes with which this user is associated, sorted by level (ADMIN first). */
  def volumeAccess = VolumeAccess.getVolumes(party)

  def setAvatar(file : play.api.libs.Files.TemporaryFile, format : AssetFormat, name : Option[String] = None)  : Future[FileAsset] =
    for {
      asset <- FileAsset.create(Volume.Core, format, Classification.PUBLIC, name, file)
      _ <- Audit.changeOrAdd("avatar", SQLTerms('asset -> asset.id), SQLTerms('party -> party.id)).execute
    } yield asset

  def pageName = party.pageName
  def pageParent = party.pageParent
  def pageURL = party.pageURL

  def json = party.json - "email" ++ JsonObject.flatten(
    Some(('permission, permission)),
    party.account.filter(_ => checkPermission(Permission.SHARED)).map(a => ('email, a.email))
  )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options
    , "parents" -> { opt =>
        val full = checkPermission(Permission.ADMIN)
        party.authorizeParents(full)
        .map(JsonArray.map(a =>
          (if (full) a.json else JsonObject()) + ('party -> a.parent.json)))
      }
    , "children" -> { opt =>
        val full = checkPermission(Permission.ADMIN)
        party.authorizeChildren(full)
        .map(JsonArray.map(a =>
          (if (full) a.json else JsonObject()) + ('party -> a.child.json)))
      }
    , "access" -> (opt => if (checkPermission(Permission.ADMIN)) party.access.map(a => Json.toJson(a.site)) else async(JsNull))
    , "volumes" -> (opt => volumeAccess.map(JsonArray.map(_.json - "party")))
    , "comments" -> (opt => party.account.fold[Future[Seq[Comment]]](async(Nil))(_.comments)
        .map(JsonArray.map(c => c.json - "who" - "container" +
          ('container -> (c.container.json - "volume" + ('volume -> c.volume.json)))))
      )
    , "openid" -> (opt => async(if (party === site.identity || site.superuser)
        Json.toJson(party.account.flatMap(_.openid)) else JsNull))
    )
}

/** Refines Party for individuals with registered (but not necessarily authorized) accounts on the site. */
final class Account protected (val party : Party, private[this] var email_ : String, private[this] var password_ : String, private[this] var openid_ : Option[URL])
  extends TableRowId[Party] {
  def ===(a : Party) = party === a

  party.account_ = Some(this)

  val id : Account.Id = party.id
  def email = email_
  /** Crypted password, using standard unix format, currently \$2a\$-style bcrypt */
  def password = password_
  def openid = openid_

  /** Update the given values in the database and this object in-place. */
  def change(email : Option[String] = None, password : Option[String] = None, openid : Option[Option[URL]] = None)(implicit site : Site) : Future[Boolean] = {
    (if (password.exists(!_.equals(password_)))
      clearTokens(cast[AuthSite](site).map(_.token))
    else async(false)).flatMap { _ =>
    Audit.change("account", SQLTerms.flatten(email.map('email -> _), password.map('password -> _), openid.map('openid -> _)), SQLTerms('id -> id))
      .execute.andThen { case scala.util.Success(true) =>
        email.foreach(email_ = _)
        password.foreach(password_ = _)
        openid.foreach(openid_ = _)
      }
    }
  }

  /** List of comments by this individual.
    * This checks permissions on the target volumes. */
  def comments(implicit site : Site) = Comment.getParty(this)

  /** Remove any issued login tokens for this user. */
  def clearTokens(except : Option[Token] = None) = AccountToken.clearAccount(id, except)

  def recentAttempts : Future[Long] =
    SQL("SELECT count(*) FROM ONLY audit.audit WHERE audit_action = 'attempt' AND audit_user = ? AND audit_time > CURRENT_TIMESTAMP - interval '1 hour'")
    .apply(id).single(SQLCols[Long])
}

object Party extends TableId[Party]("party") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Option[Orcid]]("orcid")
    , SelectColumn[Option[String]]("affiliation")
    , SelectColumn[Option[URL]]("url")
    ).map { (id, name, orcid, affiliation, url) =>
      new Party(id, name, orcid, affiliation, url)
    }
  private[models] val row : Selector[Party] =
    columns.leftJoin(Account.columns, using = 'id)
    .map { case (party, acct) =>
      acct.foreach(_(party))
      party
    }

  private[models] def getStatic(i : Id)(implicit site : Site) : Option[Party] = i match {
    case NOBODY => Some(Nobody)
    case ROOT => Some(Root)
    case i if i === site.identity.id => Some(site.identity)
    case _ => None
  }
  private[models] def _get(i : Id) : Future[Party] =
    row.SELECT("WHERE id = ?").apply(i).single
  /** Look up a party by id. */
  def get(i : Id)(implicit site : Site) : Future[Option[Party]] =
    getStatic(i).fold {
      row.SELECT("WHERE id = ?").apply(i).singleOpt
    } { p =>
      async(Some(p))
    }

  /* Only used by authorizeAdmin */
  def getAll : Future[Seq[Authorization]] =
    row
    .leftJoin(Authorization.columns, "authorize_view.child = party.id AND authorize_view.parent = 0")
    .map { case (a, p) => Authorization.make(a)(p) }
    .SELECT("ORDER BY site DESC NULLS LAST, member DESC NULLS LAST, account.id IS NOT NULL, password <> '', name").apply().list

  /** Create a new party. */
  def create(name : String, orcid : Option[Orcid] = None, affiliation : Option[String] = None, url : Option[URL] = None)(implicit site : Site) : Future[Party] =
    Audit.add("party", SQLTerms(
      'name -> name,
      'orcid -> orcid,
      'affiliation -> affiliation,
      'url -> url),
      "id")
    .single(SQLCols[Id]).map(new Party(_, name, orcid, affiliation, url))

  private def byName(implicit site : Site) =
    if (site.access.site >= Permission.SHARED)
      "(name ILIKE ? OR email ILIKE ?)"
    else
      "name ILIKE ?"
  private def byNameArgs(name : String)(implicit site : Site) =
    SQLArgs(name.split("\\s+").filter(!_.isEmpty).mkString("%","%","%")) *
      (if (site.access.site >= Permission.SHARED) 2 else 1)

  /** Search for parties
    * @param query string to match against name/email (case insensitive substring)
    * @param access limit to users with specific site access level
    * @param institution limit to institutions
    * @param authorize party doing the authorization, to exclude parties already authorized
    * @param volume volume to which to grant access, to exclude parties with access already.
    */
  def search(query : Option[String], access : Option[Permission.Value] = None, institution : Option[Boolean] = None, authorize : Option[Party] = None, volume : Option[Volume] = None)(implicit site : Site) : Future[Seq[Party]] =
    row.SELECT(if (access.nonEmpty) "JOIN authorize_view ON party.id = child AND parent = 0" else "",
      "WHERE id > 0",
      if (query.nonEmpty) "AND " + byName else "",
      if (access.nonEmpty) "AND site = ?" else "",
      institution match {
        case None => ""
        case Some(false) => "AND account.password IS NOT NULL"
        case Some(true) => "AND account.id IS NULL"
      },
      if (authorize.nonEmpty) "AND id != ? AND id NOT IN (SELECT child FROM authorize WHERE parent = ? UNION SELECT parent FROM authorize WHERE child = ?)" else "",
      if (volume.nonEmpty) "AND id NOT IN (SELECT party FROM volume_access WHERE volume = ?)" else "")
    .apply(query.fold(SQLArgs())(byNameArgs(_)) ++ access.fold(SQLArgs())(SQLArgs(_)) ++ authorize.fold(SQLArgs())(a => SQLArgs(a.id) * 3) ++ volume.fold(SQLArgs())(v => SQLArgs(v.id)))
    .list

  /** The special party group representing everybody (including anonymous users) on the site.
    * This is also in the database, but is cached here for special handling. */
  final val Nobody = new Party(asId(-1), "Everybody", None, None)
  /** The special party group representing authorized users on the site.
    * This is also in the database, but is cached here for special handling. */
  final val Root   = new Party(asId(0),  "Databrary", None, None)
  final val NOBODY : Id = Nobody.id
  final val ROOT   : Id = Root.id
}

object SiteParty {
  private[models] def row(implicit site : Site) =
    Party.row
    .leftJoin(Authorization.columns, "authorize_view.parent = party.id AND authorize_view.child = ?")
    .pushArgs(SQLArgs(site.identity.id))
    .map {
      case (party, access) => new SiteParty(Authorization.make(site.identity, party)(access))
    }

  def get(p : Party)(implicit site : Site) : Future[SiteParty] =
    if (p.id === Party.ROOT) async(new SiteParty(site.access))
    else if (p.id == Party.NOBODY) async(new SiteParty(new Authorization(site.identity, Party.Nobody, site.access.site, Permission.NONE)))
    else Authorization.get(site.identity, p).map(new SiteParty(_))

  def get(i : Party.Id)(implicit site : Site) : Future[Option[SiteParty]] =
    Party.getStatic(i).fold {
      row.SELECT("WHERE party.id = ?").apply(i).singleOpt
    } (get(_).map(Some(_)))
}

object Account extends Table[Account]("account") {
  type Id = Party.Id
  private[models] val columns = Columns(
      SelectColumn[String]("email")
    , SelectColumn[Option[String]]("password")
    , SelectColumn[Option[URL]]("openid")
    ).map { (email, password, openid) =>
      (party : Party) => new Account(party, email, password.getOrElse(""), openid)
    }
  private[models] val row : Selector[Account] =
    Party.columns.join(columns, using = 'id) map {
      case (party, acct) => acct(party)
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

  def create(party : Party, email : String, password : Option[String] = None, openid : Option[URL] = None)(implicit site : Site) : Future[Account] =
    Audit.add("account", SQLTerms('id -> party.id, 'email -> email, 'password -> password, 'openid -> openid)).map { _ =>
      new Account(party, email, password.getOrElse(""), openid)
    }
}
