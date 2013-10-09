package site

import dbrary.cast
import models._
import play.api.mvc._
import scala._
import scala.Tuple3
import scala.Some

object Site {
  type DB = java.sql.Connection
}
/** Basic information about each request.  Primarily implemented by [[controllers.SiteRequest]]. */
trait Site {
  /** [[models.Party]] of the logged-in user, possibly [[models.Party.Nobody]]. */
  val identity : models.Party
  /** Database connection (with active transaction). */
  val db : Site.DB
  /** Some([[identity]]) only if actual logged-in user. */
  def user : Option[models.Account] = cast[models.Account](identity)
  /** Level of site access [[models.Permission]] current user has.
    * VIEW for anonymous, DOWNLOAD for affiliate, CONTRIBUTE for authorized, ADMIN for admins.
    */
  def access = identity.access(db)
  def isAdmin = access >= models.Permission.ADMIN
  /** IP of the client's host. */
  def clientIP : dbrary.Inet
}

trait AnonSite extends Site {
  val identity = models.Party.Nobody
  override def user = None
}

trait AuthSite extends Site {
  val identity : models.Account
  override def user = Some(identity)
}

/** An object with a corresponding page on the site. */
trait SitePage { self =>
  /** The title of the object/page in the hierarchy, which may only make sense within [[pageParent]]. */
  def pageName(implicit site : Site) : String
  /** The object "above" this one (in terms of breadcrumbs and nesting). */
  def pageParent(implicit site : Site) : Option[SitePage]
  /** The URL of the page, usually determined by [[controllers.routes]]. */
  def pageURL(implicit site : Site) : play.api.mvc.Call
  /** The actions available for this page, as (action, route, permission) */
  def pageActions(implicit site : Site) : Seq[Tuple3[String, play.api.mvc.Call, Permission.Value]]
}
