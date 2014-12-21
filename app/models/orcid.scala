package models

import play.api.libs.json
import play.api.data.format.Formatter
import dbrary._

/** An [[http://orcid.org/ ORCID]] identifier.
  * A 16 character string of a particular format, with 15 digits and one checksum which may be a digit or 'X'?
  * They are stored as a string of length 16, but displayed with dashes.
  */
final class Orcid private (val orcid : String)
{
  /** Determine if this is a valid ORCID. */
  def valid : Boolean = orcid.lengthCompare(16) == 0 && {
    val (b, cs) = orcid.splitAt(15)
    val d = b.map(_.asDigit)
    d.forall(i => i >= 0 && i < 10) && {
      val c = 10 - ((d.foldLeft(0)(2*_ + 2*_) + 9) % 11)
      cs.head == (if (c == 10) 'X' else '0' + c)
    }
  }

  override def toString : String = orcid.grouped(4).mkString("-")
  /** The URI for this ORCID, which should be used for linking and often display. */
  def uri : String = "http://orcid.org/" + toString
}

object Orcid {
  /** Create an [[Orcid]], possibly removing any formatting cruft, without checking for validity. */
  def apply(s : String) : Orcid =
    new Orcid(s.filterNot(c => c == '-' || c.isSpaceChar).stripPrefix("http://").stripPrefix("orcid.org/"))

  implicit object formatter extends Formatter[Orcid] {
    override val format = Some(("format.orcid", Nil))
    def bind(key: String, data: Map[String, String]) = {
      val orcid = apply(data.get(key).getOrElse(""))
      if (orcid.valid) Right(orcid)
      else Left(Seq(play.api.data.FormError(key, "orcid.invalid", Nil)))
    }
    def unbind(key: String, value: Orcid) = Map(key -> value.toString)
  }

  implicit val sqlType : SQL.Type[Orcid] =
    SQL.Type[Orcid]("char(16)", classOf[Orcid])(s => Some(new Orcid(s)), _.orcid)

  implicit val jsWrites : json.Writes[Orcid] =
    json.Writes[Orcid](o => json.JsString(o.toString))
}
