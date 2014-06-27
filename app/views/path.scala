package views.html

/**
 * The file needs to be sorted to views/script/
 */

import play.twirl.api.{Html,HtmlFormat}
import site._
import models._
import controllers._

final class PathCrumb(name : String, url : play.api.mvc.Call, li : Boolean = false) {
  override def toString = name

  def toHtml : Html = {
    import HtmlFormat._
    val l = collection.immutable.Seq(
      raw("<a href='"),
      raw(url.toString),
      raw("'>"),
      escape(name),
      raw("</a>"))
    fill(if (li) raw("<li>") +: l :+ raw("</li>") else l)
  }
}

object PathCrumb {
  def apply(x : SitePage, li : Boolean = false)(implicit site : Site): PathCrumb = new PathCrumb(x.pageCrumbName.getOrElse(x.pageName), x.pageURL, li)
}

final class Path private(val parts : collection.immutable.Seq[PathCrumb]) {
  def ++(t: Path) = new Path(parts ++ t.parts)

  def :+(c: PathCrumb) = new Path(parts :+ c)

  override def toString = parts.mkString(" :: ")

  def toHtml : Html = HtmlFormat.fill(parts.map(_.toHtml))
}

object Path {
  def apply(c : PathCrumb*) : Path = new Path(collection.immutable.Seq(c : _*))
  def apply(x : SitePage)(implicit site : Site) : Path = x.pageParent.fold(Path())(Path(_)) :+ PathCrumb(x, true)
}
