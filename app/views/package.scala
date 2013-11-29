package views.html

import play.api.templates.HtmlFormat._
import site._
import models._
import controllers._
import java.security.MessageDigest

case class El(tag: String = "", attrs: List[List[String]])(content: String = "") {
  override def toString = content

  def toHtml = {
    val r = raw("<"+raw(tag)+" "+attrs.map(attr => raw(attr(0))+"=\""+escape(attr(1))+"\"").mkString(" ")+">"+escape(content)+"</"+raw(tag)+">")
    r
  }
}

object El {
  def attrToHtml(attr: Seq[(String,String)]) = {raw(attr.toMap.map(attr => if(attr._2.nonEmpty){attr._1+"=\""+attr._2+"\""}else{attr._1}).mkString(" "))}
}
