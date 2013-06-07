package util

object maybe {
  def apply(s : String) =
    if (s.isEmpty) None else Some(s)
}

trait Site {
  val identity : models.Identity
  val db : slick.session.Session
  def access = identity.access(db)
  def clientIP : models.Inet
}
