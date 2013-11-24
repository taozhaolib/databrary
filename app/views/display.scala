package views.html
/**
 * The file needs to be sorted to views/script/
 */

import play.api.templates.HtmlFormat._
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
      case Seq(y, _, _) => (y, "yrs")
      case _ => (0, "???")
    }
    "%.1f %s".format(n, u)
  }
  def age(record : models.Record, slot : models.Slot) : Option[String] =
    slot.container.date.flatMap(d => record.age(d)).map(age _)

  def agerange(a : dbrary.Range[Age]) : String = range(age)(a)

  val dateFmtY    = DateTimeFormat.forPattern("yyyy")
  val dateFmtYM   = DateTimeFormat.forPattern("MMMM yyyy")
  val dateFmtYMD  = DateTimeFormat.forPattern("yyyy-MMM-dd")
  val dateFmtCite = DateTimeFormat.forPattern("MMMM d, YYYY")

  private def fuzzyDate(date : org.joda.time.ReadablePartial) =
    if (date.isInstanceOf[Date]) dateFmtYMD.print(date) else date.toString

  def date(s : Slot) =
    s.getDate.map(fuzzyDate _)

  def formatTitle(text: String = "") =
    raw(text.replaceAll(": ", ": <br>"))

  def plainText(text: String = "") =
    raw("<p>"+text.replaceAll("\\r?\\n\\r?\\n", "</p><p>")+"</p>")

  def plainTextSummary(text: String = "", length: Int = 3) =
    raw("<p>"+text.split("\\r?\\n\\r?\\n").take(length).mkString("</p><p>")+"</p>")

  private def gravatarUrlByEmailOpt(email: Option[String] = None, size: Int = 64) =
    "http://gravatar.com/avatar/"+email.fold("none")(e => md5(e.toLowerCase.replaceAll("\\s+", "")).hash)+"?s="+size+"&d=mm"

  private def gravatarUrlByEmail(email: String, size: Int = 64) =
    gravatarUrlByEmailOpt(Some(email), size)

  private def gravatarUrlByParty(party: Party, size: Int = 64) =
    gravatarUrlByEmailOpt(party.account.map(_.email), size)

  def avatar(party : Party, size : Int = 64) = party.name match {
    /* Temporary hack */
    case "Karen Adolph" => routes.Assets.at("private/profiles/karen.jpg")
    case "Rick Gilmore" => routes.Assets.at("private/profiles/rick.jpg")
    case "David Millman" => routes.Assets.at("private/profiles/david.jpg")
    case "Catherine Tamis-LeMonda" => routes.Assets.at("private/profiles/cathy.jpg")
    case "Dylan Simon" => routes.Assets.at("private/profiles/dylan.jpg")
    case "Lisa Steiger" => routes.Assets.at("private/profiles/lisa.jpg")
    case "Lina Wictoren Roy" => routes.Assets.at("private/profiles/lina.jpg")
    case "Andrea Byrne" => routes.Assets.at("private/profiles/andrea.jpg")
    case "National Institutes of Health" => routes.Assets.at("private/profiles/nih.jpg")
    case "National Institute of Child Health and Human Development" => routes.Assets.at("private/profiles/nih.jpg")
    case "National Science Foundation" => routes.Assets.at("private/profiles/nsf.png")
    case "Databrary" => routes.Assets.at("private/profiles/databrary.png")
    case "New York University" => routes.Assets.at("private/profiles/nyu.jpg")
    case _ => gravatarUrlByParty(party, size)
  }

  def citeName(name: String) = {
    val names = name.split(" +")
    names.last + (if (names.length > 1) {
      ", " + names.init.map(_.head + ".").mkString(" ")
    } else "")
  }

  def apply(x : SitePage, full : Boolean = false)(implicit site : Site) = if (full) path(x) else page(x)
  def apply(x : Timestamp) = time(x)
  def apply(x : Range[Offset]) = x.singleton.fold(x.lowerBound.fold("")(_.toString) + "-" + x.upperBound.fold("")(_.toString))(_.toString)
}
