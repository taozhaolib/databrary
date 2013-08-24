package util

import dbrary._

/** Utility that creates [[scala.Option]]s out of values. */
object maybe {
  /** A more concise version of the common `Some(_).filter(_)` idiom.
    * @return Some(a) if f(a), None otherwise
    */
  def apply[A](a : A, f : A => Boolean) : Option[A] =
    Some(a).filter(f)

  /** Eliminate a specific value, like SQL's `NULLIF`.
    * @return Some(a) unless a == n
    */
  def apply[A](a : A, n : A) : Option[A] =
    maybe(a, (_ : A) != n)
  /** Default empty value for Strings. */
  def apply(s : String, f : String => Boolean = !_.isEmpty) =
    Some(s).filter(f)
}

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
  /** IP of the client's host. */
  def clientIP : dbrary.Inet
}
