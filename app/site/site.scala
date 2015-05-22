package site

import scala.concurrent.Future
import play.api.Play
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import macros.async._
import dbrary._
import models._
import scala._

object Site {
  dbrary.init()

  private val properties = new java.util.Properties
  Option(getClass.getResourceAsStream("/properties")).foreach(properties.load)
  def appName = properties.getProperty("name", "unknown")
  val version = properties.getProperty("version", "unknown") + (if (!Play.isProd(Play.current)) "-" + Play.current.mode else "")
  val appVersion = appName + "/" + version

  type DB = com.github.mauricio.async.db.Connection
  lazy val dbPool : DB =
    Play.current.plugin[PostgresAsyncPlugin].fold(throw new Exception("PostgresAsyncPlugin not registered"))(_.pool)

  lazy val accessLog =
    Play.current.plugin[org.databrary.LogbackAccessPlugin].fold(throw new Exception("LogbackAccessPlugin not registered"))(_.api)

  val url = Play.current.configuration.getString("site.url").getOrElse("")
  def url(call : Call) : String =
    url + call.url

  val sandbox = Play.current.configuration.getBoolean("site.sandbox").getOrElse(false)

  periodic.start()
}

/** An effective authorization of identity by target. */
trait Access {
  /** The user/group to which this user is granted access.  The "parent" in the authorization relationship. */
  def target : Party
  /** The user granted this access level.  The "child" in the authorization relationship. */
  def identity : Party
  /** The inherited level of group access within target ("site" access when target == Root). */
  val site : Permission.Value
  /** The direct level of access granted to target's data. */
  val member : Permission.Value
  /** The level of delegated access identity has over the target party itself. */
  def permission = min(site, member)
  def isAdmin = permission == Permission.ADMIN
}

/** Basic information about each request.  Primarily implemented by [[controllers.SiteRequest]]. */
trait Site {
  def access : Access
  assert(access.target == Party.Root)
  /** [[models.Party]] of the logged-in user, possibly [[models.Party.Nobody]]. */
  final def identity : Party = access.identity
  /** Some(identity) only if actual logged-in user. */
  def user : Option[models.Account] = identity.account
  val superuser : Boolean
  /** IP of the client's host. */
  val clientIP : dbrary.Inet

  final def json =
    identity.json(this) ++
    JsonObject.flatten(
      Some('access -> access.site)
    , if (access.isAdmin) Some('superuser -> superuser) else None
    )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options
    , "volumes" -> (opt => Volume.getAccess()(this).flatMap(_.mapAsync[JsonRecord, Seq[JsonRecord]](_.json(Map("access" -> Nil))).map(JsonArray(_))))
    )
}

trait AnonSite extends Site {
  final def access = Authorization.Nobody
  final val superuser = false
}

trait AuthSite extends Site {
  val token : SessionToken
  def account : Account = token.account
  override def user = Some(account)
  final def access = token.access
}

object AnonSite extends AnonSite {
  val clientIP = Inet("0.0.0.0")
}

final class LocalAuth(val access : Access, val superuser : Boolean = false) extends Site {
  val clientIP = Inet("0.0.0.0")
}

trait PerSite {
  implicit def site : Site
}

/** An object with a corresponding page on the site. */
trait SitePage {
  /** The title of the object/page in the hierarchy, which may only make sense within [[pageParent]]. */
  def pageName : String
  /** Optional override of pageName for breadcrumbs and other abbreviated locations */
  def pageCrumbName : Option[String] = None
  /** The object "above" this one (in terms of breadcrumbs and nesting). */
  def pageParent : Option[SitePage]
  /** The URL of the page, usually determined by [[controllers.routes]]. */
  def pageURL : play.api.mvc.Call
}

trait SiteObject extends SitePage with HasPermission {
  def json : JsonValue
}
