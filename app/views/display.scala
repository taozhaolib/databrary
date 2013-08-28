package views.html
/**
 * The file needs to be sorted to views/script/
 */

import play.api.templates.HtmlFormat._
import util._
import models._
import controllers._
import scala.Some

object display {
  def page(page : SitePage)(implicit site : Site) = PathCrumb(page).toHtml
  def path(page : SitePage)(implicit site : Site) = Path(page).toHtml

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

  def plainText(text: Option[String] = Option("")) = {
    raw("<p>"+text.getOrElse("").split("\\r?\\n").mkString("</p><p>")+"</p>")
  }

  def gravatarUrl(email: String = "none", size: Int = 64) = {
    "http://gravatar.com/avatar/"+md5(email.toLowerCase.replaceAll("\\s+", "")).hash+"?s="+size+"&d=mm"
  }

  def apply(x : SitePage, full : Boolean = false)(implicit site : Site) = if (full) path(x) else page(x)
  def apply(x : java.util.Date) = time(x)
}
