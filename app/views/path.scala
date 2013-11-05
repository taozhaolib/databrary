package views.html

/**
 * The file needs to be sorted to views/script/
 */

import play.api.templates.HtmlFormat._
import site._
import models._
import controllers._

case class PathCrumb(name: String, url: Option[play.api.mvc.Call] = None, li: Boolean = false) {
  override def toString = name

  def toHtml = {
    val r = raw("")
    if(li) r += raw("<li>")
    r += raw("<a")
    r += raw(" href='" + url.getOrElse("").toString + "'")
    r += raw(">")
    r += escape(if(name.length >= 32) name.substring(0, 29).trim+"..." else name)
    r += raw("</a>")
    if(li) r += raw("</li>")
    r
  }
}

object PathCrumb {
  def apply(name: String, url: play.api.mvc.Call): PathCrumb = PathCrumb(name, Some(url))

  def apply(x: SitePage)(implicit site: Site): PathCrumb = PathCrumb(x.pageCrumbName.getOrElse(x.pageName), Some(x.pageURL))
  def apply(x: SitePage, li: Boolean)(implicit site: Site): PathCrumb = PathCrumb(x.pageCrumbName.getOrElse(x.pageName), Some(x.pageURL), li)
}

final class Path private(parts: Seq[PathCrumb]) extends scala.collection.generic.SeqForwarder[PathCrumb] {
  def underlying = parts

  def ++(t: Path) = new Path(parts ++ t)

  def :+(c: PathCrumb) = new Path(parts :+ c)

  override def toString = parts.mkString(" :: ")

  def toHtml = parts match {
    case h :: l => l.foldLeft(h.toHtml) {
      (r, x) => r += x.toHtml
    }
    case Nil => raw("")
  }
}

object Path {
  def apply(c: PathCrumb*): Path = new Path(c)

  def apply(x: SitePage)(implicit site: Site): Path = x.pageParent.fold(Path())(Path(_)) :+ PathCrumb(x, true)
}
