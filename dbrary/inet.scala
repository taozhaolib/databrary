package dbrary

case class Inet(ip : String)
object Inet {
  implicit val sqlType : SQL.Type[Inet] =
    SQL.Type[Inet]("inet", classOf[Inet])(s => Some(Inet(s)), _.ip)
}
