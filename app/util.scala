package util {

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

}

package object util {
  implicit def siteDB(implicit site : Site) : Site.DB = site.db

  /** Group adjacent elements with identical keys into nested lists, such that the concatenation of the resulting _2 elements is the original list.
    * This is more like Haskell's group than scala's [[Seq#groupBy]]: only adjacent elements are grouped.
    * @param l the list to group
    * @param f the key-generating function to group by
    */
  def groupBy[A,K](l : Seq[A], f : A => K) : Seq[(K,Seq[A])] = {
    val r = l.genericBuilder[(K,Seq[A])]
    @scala.annotation.tailrec def next(l : Seq[A]) : Unit = if (l.nonEmpty) {
      val k = f(l.head)
      val (p, s) = l.span(f(_) == k)
      r += k -> p
      next(s)
    }
    next(l)
    r.result
  }
}
