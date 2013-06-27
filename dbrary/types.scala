package dbrary

import org.postgresql._
import org.postgresql.util._

abstract class PGtype[S](pgType : String, pgValue : String) extends PGobject {
  setType(pgType)
  setValue(pgValue)
  def unPG : S
}

class PGType[PG <: PGtype[S], S](pg : S => PG) {
  import scala.language.implicitConversions
  implicit def pgToScala(pg : PG) : S = pg.unPG
  implicit def scalaToPg(s : S) : PG = pg(s)
}


case class Inet(ip : String)

final class PGinet(s : Inet) extends PGtype[Inet]("inet", s.ip) {
  def unPG = Inet(value)
  def this() = this(Inet(""))
}
object PGinet extends PGType[PGinet,Inet](new PGinet(_))

/*
object PGBoot {
  val pgTypes = Seq(
    "inet" -> classOf[PGinet]
  )

  def init(db : PGConnection) = {
    for ((t, c) <- pgTypes) {
      db.addDataType(t, c)
    }
  }
}
*/
