package views.html
/**
 * The file needs to be sorted to views/script/
 */

import play.api.templates.HtmlFormat._
import util._
import models._
import controllers._
import java.text.SimpleDateFormat
import java.util.Date

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
    val (_, s) = ((a, "") /: timeUnits.take(3)) { (as, ut) =>
      val (a, s) = as
      val (u, t) = ut
      val n = a / t
      (a - n*t, if (n != 0) s + n + u(0) else s)
  }
    if (s.isEmpty) "0" else s
  }

  def agerange(a : dbrary.Range[Long]) : String = range(age)(a)

  private val dateFmtY = new SimpleDateFormat("yyyy")
  private val dateFmtYM = new SimpleDateFormat("MMMM yyyy")
  private val dateFmtYMD = new SimpleDateFormat("yyyy-MMM-dd")

  def date(t : java.util.Date, format : String) : String =
    new SimpleDateFormat(format).format(t)

  def date(s : Slot)(implicit site : Site) =
    (if (s.dataAccess() >= Permission.DOWNLOAD) dateFmtYMD else dateFmtY).format(s.date)

  def plainText(text: String = "") =
    raw("<p>"+text.split("\\r?\\n").mkString("</p><p>")+"</p>")

  def plainTextSummary(text: String = "", length: Int = 1000) = {
    raw("<p>"+text.split("\\r?\\n").slice(0, length).mkString("</p><p>")+"</p>")
  }

  def gravatarUrlByEmail(email: String = "none", size: Int = 64) = {
    "http://gravatar.com/avatar/"+md5(email.toLowerCase.replaceAll("\\s+", "")).hash+"?s="+size+"&d=mm"
  }

  def gravatarUrlByParty(party: Party, size: Int = 64) = {
    dbrary.cast[Account](party) match {
      case Some(account) => "http://gravatar.com/avatar/"+md5(account.email.toLowerCase.replaceAll("\\s+", "")).hash+"?s="+size+"&d=mm"
      case None => "http://gravatar.com/avatar/none?s="+size+"&d=mm"
    }
  }

  def citeName(name: String) = {
    var names = name.split(" ")

    var out = names.last+", "
    names = names.take(names.length - 1)

    for (n <- 0 until names.length) {
      out += ""+names(n).substring(0,1)+"."
    }

    out
  }

  def apply(x : SitePage, full : Boolean = false)(implicit site : Site) = if (full) path(x) else page(x)
  def apply(x : java.util.Date) = time(x)
}
