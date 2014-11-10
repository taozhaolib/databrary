package views.html
/**
 * The file needs to be sorted to views/script/
 */

import play.api.mvc.RequestHeader
import play.twirl.api.HtmlFormat._
import org.joda.time.format.DateTimeFormat
import macros._
import site._
import models._
import controllers._
import dbrary._

object display {
  def page(page : SitePage)(implicit site : Site) = PathCrumb(page).toHtml
  def path(page : SitePage)(implicit site : Site) = Path(page).toHtml

  private[this] def range[A](f : A => String)(r : dbrary.Range[A]) : String =
    if (r.isEmpty) "" else r.singleton.fold(r.lowerBound.fold("")(f) + "-" + r.upperBound.fold("")(f))(f)

  /* roughly: */
  private[this] final val timeUnits = Seq[(String,Long)](
      "year" -> 31556926000L,
      "month" -> 2629743800L,
      "day" -> 86400000,
      "hour" -> 3600000,
      "minute" -> 60000,
      "second" -> 1000
    )
  def time(t : org.joda.time.ReadableInstant) : String = {
    val d = t.getMillis - System.currentTimeMillis
    val a = d.abs
    timeUnits.find(a >= _._2).fold("0 seconds") { case (u, t) =>
      val n = a / t
      n + " " + u + (if (n == 1) "" else "s")
    } + (if (d < 0) " ago" else "")
  }

  def age(a : Age) : String = {
    val (n, u) = timeUnits.take(3).map(a.millis.toDouble/_._2.toDouble) match {
      case Seq(_, m, d) if m < 3 => (d, "dys")
      case Seq(_, m, _) if m < 37 => (m, "mos")
      case Seq(y, _, _) if y >= 90 => (90, "yrs or older")
      case Seq(y, _, _) => (y, "yrs")
      case _ => (0, "???")
    }
    "%.1f %s".format(n, u)
  }
  def age(record : models.Record, slot : models.Slot) : Option[String] =
    record.age(slot).map(age _)

  def agerange(a : dbrary.Range[Age]) : String = range(age)(a)

  val dateFmtY    = DateTimeFormat.forPattern("yyyy")
  val dateFmtYM   = DateTimeFormat.forPattern("MMMM yyyy")
  val dateFmtYMD  = DateTimeFormat.forPattern("yyyy-MMM-dd")
  val dateFmtCite = DateTimeFormat.forPattern("MMMM d, YYYY")

  private def fuzzyDate(date : org.joda.time.ReadablePartial) =
    if (date.isInstanceOf[Date]) dateFmtYMD.print(date)
    else date.toString

  def date(s : Slot) =
    s.getDate.map(fuzzyDate _)

  def formatTitle(text: String = "") =
    raw(escape(text).body)

  def rawFormat(text : String) =
    raw("<p>"+text.replaceAll("\\n\\n", "</p><p>")+"</p>")

  def plainText(text: String = "") =
    rawFormat(escape(text).body)

  def plainTextSummary(text: String = "", length: Int = 3) =
    raw("<p>"+escape(text).body.split("\\r?\\n\\r?\\n").take(length).mkString("</p><p>")+"</p>")

  def avatar(party : Party, size : Int = 64) =
    routes.PartyHtml.avatar(party.id, size)

  def citeName(name: String) = {
    val names = name.split(" +")
    names.last + (if (names.length > 1) {
      ", " + names.init.map(_.head + ".").mkString(" ")
    } else "")
  }

  def apply(x : SitePage, full : Boolean = false)(implicit site : Site) = if (full) path(x) else page(x)
  def apply(x : Timestamp) = time(x.toDateTime)
  def apply(x : Range[Offset]) = x.singleton.fold(x.lowerBound.fold("")(_.toString) + "-" + x.upperBound.fold("")(_.toString))(_.toString)

  def url(request : RequestHeader, abs : Boolean = false) : String =
    (if (abs) "http" + (if (request.secure) "s" else "") + "://" + request.host else "") +
    request.path +
    (if (request.queryString.isEmpty) "" else
      "?" + request.queryString.toSeq.flatMap { case (key, values) =>
        values.map(value => (key + "=" + java.net.URLEncoder.encode(value, "utf-8")))
      }.mkString("&"))
}
