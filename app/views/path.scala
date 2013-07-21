package views.html

import play.api.templates.HtmlFormat._
import util._
import models._
import controllers._

case class PathCrumb(name : String, url : Option[String] = None) {
  override def toString = name
  def toHtml = url.fold(escape(name)) { url => 
    val r = raw("<a href='")
    r += escape(url)
    r += raw("'>")
    r += escape(name)
    r += raw("</a>")
    r
  }
}
object PathCrumb {
  def apply(name : String, url : String) : PathCrumb = PathCrumb(name, Some(url))
  def apply(x : SitePage)(implicit site : Site) : PathCrumb = PathCrumb(x.pageName, Some(x.pageURL))
}

final class Path private (parts : Seq[PathCrumb]) extends scala.collection.generic.SeqForwarder[PathCrumb] {
  def underlying = parts
  def ++(t : Path) = new Path(parts ++ t)
  def :+(c : PathCrumb) = new Path(parts :+ c)
  override def toString = parts.mkString(": ")
  def toHtml = parts match {
    case h :: l => l.foldLeft(h.toHtml) { (r,x) => r += raw(" &gt; "); r += x.toHtml }
    case Nil => raw("")
  }
}

object Path {
  def apply(c : PathCrumb*) : Path = new Path(c)
  def apply(x : SitePage)(implicit site : Site) : Path = x.pageParent.fold(Path())(Path(_)) :+ PathCrumb(x)
}
