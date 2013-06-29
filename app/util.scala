package util

object maybe {
  def apply(s : String) =
    if (s.isEmpty) None else Some(s)
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
