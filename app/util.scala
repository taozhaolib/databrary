package util

object maybe {
  /* is this in the standard library somewhere? could be generalized as "when"/comprehension guard */
  def apply[A](a : A, f : A => Boolean) : Option[A] =
    Some(a).filter(f)
  /* special default for strings */
  def apply(s : String, f : String => Boolean = !_.isEmpty) =
    Some(s).filter(f)
}

object Site {
  type DB = java.sql.Connection
}
trait Site {
  val identity : models.Identity
  val db : Site.DB
  def access = identity.access(db)
  def clientIP : dbrary.Inet
}
