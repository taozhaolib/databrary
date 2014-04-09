package media

object Passwd {
  init

  @native def _check(passwd : String, user : String, name : String) : String
  def check(passwd : String, user : String, name : String) : Option[String] =
    Option(_check(passwd, user, name))
}
