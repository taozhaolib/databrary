package util

import dbrary._

object maybe {
  /* A more concise version of the common Option(_).filter(_) idiom */
  def apply[A](a : A, f : A => Boolean) : Option[A] =
    Some(a).filter(f)
  /* special default for strings */
  def apply(s : String, f : String => Boolean = !_.isEmpty) =
    Some(s).filter(f)
}

object Site {
  type DB = java.sql.Connection
}
/* The basic information in every request, primarily implemented by controllers.SiteRequest */
trait Site {
  val identity : models.Entity
  val db : Site.DB
  def user : Option[models.Account] = cast[models.Account](identity)
  def access = identity.access(db)
  def clientIP : dbrary.Inet
  def timezone : java.util.TimeZone
}
