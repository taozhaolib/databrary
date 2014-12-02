package media

object Passwd {
  init

  @native def _check(passwd : String, user : String, name : String) : String
  def check(passwd : String, user : String, name : String) : Option[String] =
    synchronized { Option(_check(passwd, user, name)) }
}
