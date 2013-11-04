package site

import dbrary.cast
import models._
import play.api.mvc._
import scala._

object Site {
  type DB = com.github.mauricio.async.db.Connection
  private def getDBPool(implicit app : play.api.Application) : DB =
    app.plugin[PostgresAsyncPlugin].fold(throw new Exception("PostgresAsyncPlugin not registered"))(_.pool)
  lazy val dbPool : DB = getDBPool(play.api.Play.current)
}
/** Basic information about each request.  Primarily implemented by [[controllers.SiteRequest]]. */
trait Site {
  /** [[models.Party]] of the logged-in user, possibly [[models.Party.Nobody]]. */
  val identity : models.Party
  /** Some(identity) only if actual logged-in user. */
  def user : Option[models.Account] = cast[models.Account](identity)
  /** Level of site access [[models.Permission]] current user has.
    * VIEW for anonymous, DOWNLOAD for affiliate, CONTRIBUTE for authorized, ADMIN for admins.
    */
  val access : Permission.Value
  val superuser : Boolean
  /** IP of the client's host. */
  def clientIP : dbrary.Inet
}

trait AnonSite extends Site {
  val identity = models.Party.Nobody
  override def user = None
  val superuser = false
  val access = models.Permission.NONE /* XXX should this be VIEW? */
}

trait AuthSite extends Site {
  val identity : models.Account
  override def user = Some(identity)
}

/** A generic action that may be performed on the site.
  * It may make sense to add a target : SitePage value here, too.
  */
case class SiteAction(name : String, route : play.api.mvc.Call, available : Boolean = true)

/** An object with a corresponding page on the site. */
trait SitePage extends HasPermission {
  /** The title of the object/page in the hierarchy, which may only make sense within [[pageParent]]. */
  def pageName(implicit site : Site) : String
  /** Optional override of pageName for breadcrumbs and other abbreviated locations */
  def pageCrumbName(implicit site : Site) : Option[String] = None
  /** The object "above" this one (in terms of breadcrumbs and nesting). */
  def pageParent(implicit site : Site) : Option[SitePage]
  /** The URL of the page, usually determined by [[controllers.routes]]. */
  def pageURL(implicit site : Site) : play.api.mvc.Call
  protected def Action(name : String, route : play.api.mvc.Call, permission : Permission.Value)(implicit site : Site) =
    SiteAction(name, route, checkPermission(permission))
  /** The actions available for this page. */
  def pageActions(implicit site : Site) : Seq[SiteAction]
}
