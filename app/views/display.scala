package views.html
/**
 * The file needs to be sorted to views/script/
 */

import play.api.templates.HtmlFormat._
import site._
import models._
import controllers._
import java.text.SimpleDateFormat
import java.util.Date
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
  def time(t : java.util.Date) : String = {
    val d = t.getTime - System.currentTimeMillis
    val a = d.abs
    timeUnits.find(a >= _._2).fold("0 seconds") { case (u, t) =>
      val n = a / t
      n + " " + u + (if (n == 1) "" else "s")
    } + (if (d < 0) " ago" else "")
  }

  def age(a : Long) : String = {
    val (n, u) = timeUnits.take(3).map(a.toDouble/_._2.toDouble) match {
      case Seq(_, m, d) if m < 3 => (d, "dys")
      case Seq(_, m, _) if m < 37 => (m, "mos")
      case Seq(y, _, _) => (y, "yrs")
      case _ => (0, "???")
    }
    "%.1f %s".format(n, u)
  }
  def age(record : models.Record, slot : models.Slot)(implicit db : site.Site.DB) : Option[String] =
    slot.container.date.flatMap(record.age _).map(age _)

  def agerange(a : dbrary.Range[Long]) : String = range(age)(a)

  val dateFmtY = new SimpleDateFormat("yyyy")
  val dateFmtYM = new SimpleDateFormat("MMMM yyyy")
  val dateFmtYMD = new SimpleDateFormat("yyyy-MMM-dd")
  val dateFmtCite = new SimpleDateFormat("MMMM d, YYYY")

  def fuzzyDate(date : Date, fuzzy : Boolean = true)(implicit site : Site) =
    (if (fuzzy) dateFmtY else dateFmtYMD).format(date)

  def date(s : Slot)(implicit site : Site) =
    s.container.date.map(fuzzyDate(_, !s.dataPermission().checkPermission(Permission.DOWNLOAD)))

  def plainText(text: String = "") =
    raw("<p>"+text.replaceAll("\\r?\\n\\r?\\n", "</p><p>")+"</p>")

  def plainTextSummary(text: String = "", length: Int = 3) =
    raw("<p>"+text.split("\\r?\\n\\r?\\n").take(length).mkString("</p><p>")+"</p>")

  private def gravatarUrlByEmailOpt(email: Option[String] = None, size: Int = 64) =
    "http://gravatar.com/avatar/"+email.fold("none")(e => md5(e.toLowerCase.replaceAll("\\s+", "")).hash)+"?s="+size+"&d=mm"

  private def gravatarUrlByEmail(email: String, size: Int = 64) =
    gravatarUrlByEmailOpt(Some(email), size)

  private def gravatarUrlByParty(party: Party, size: Int = 64) =
    gravatarUrlByEmailOpt(dbrary.cast[Account](party).map(_.email), size)

  def avatar(party : Party, size : Int = 64) = party.name match {
    /* Temporary hack */
    case _ => gravatarUrlByParty(party, size)
  }

  def citeName(name: String) = {
    val names = name.split(" +")
    names.last + (if (names.length > 1) {
      ", " + names.init.map(_.head + ".").mkString(" ")
    } else "")
  }

  def apply(x : SitePage, full : Boolean = false)(implicit site : Site) = if (full) path(x) else page(x)
  def apply(x : java.util.Date) = time(x)
  def apply(x : Range[Offset]) = x.singleton.fold(x.lowerBound.fold("")(_.toString) + "-" + x.upperBound.fold("")(_.toString))(_.toString)
}
