package models

import play.api.data.format.Formatter
import macros._
import dbrary._

/** [[http://en.wikipedia.org/wiki/DUNS D-U-N-S]].
  * A 9 digit number.
  */
final class DUNS private (val duns : Int)
{
  def valid : Boolean = duns >= 0 && duns <= 999999999
  override def toString : String = duns.formatted("%09d")
}

object DUNS {
  implicit val formatter : Formatter[DUNS] = new Formatter[DUNS] {
    override val format = Some(("format.duns", Nil))
    def bind(key: String, data: Map[String, String]) =
      (for {
	s <- data.get(key)
	n <- Maybe.toInt(s)
	duns = new DUNS(n)
	if duns.valid
      } yield(duns))
	.toRight(Seq(play.api.data.FormError(key, "duns.invalid", Nil)))
    def unbind(key: String, value: DUNS) = Map(key -> value.toString)
  }

  implicit val sqlType : SQLType[DUNS] =
    SQLType.numeric.transform[DUNS]("numeric(9)", classOf[DUNS])(
      n => Maybe.toNumber(n.toIntExact).map(new DUNS(_)),
      d => BigDecimal(d.duns)
    )
}
