package site

import dbrary.cast

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
  val superuser : Boolean
  /** IP of the client's host. */
  def clientIP : dbrary.Inet
}

trait AnonSite extends Site {
  val identity = models.Party.Nobody
  override def user = None
  val superuser = false
  override val access = models.Permission.NONE
}

trait AuthSite extends Site {
  val identity : models.Account
  override def user = Some(identity)
}

/** An object with a corresponding page on the site. */
trait SitePage {
  /** The title of the object/page in the hierarchy, which may only make sense within [[pageParent]]. */
  def pageName(implicit site : Site) : String
  /** The object "above" this one (in terms of breadcrumbs and nesting). */
  def pageParent(implicit site : Site) : Option[SitePage]
  /** The URL of the page, usually determined by [[controllers.routes]]. */
  def pageURL(implicit site : Site) : String
}
