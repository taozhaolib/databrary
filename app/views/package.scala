package views.html

import play.api.templates.HtmlFormat._
import util._
import models._
import controllers._
import java.security.MessageDigest

case class Tag(tag: String = "", attrs: List[List[String]])(content: String = "") {
  override def toString = content

  def toHtml = {
    val r = raw("<"+raw(tag)+" "+attrs.map(attr => raw(attr(0))+"=\""+escape(attr(1))+"\"").mkString(" ")+">"+escape(content)+"</"+raw(tag)+">")
    r
  }
}

case class md5(string: String) {
  def hash = {
    val md5 = MessageDigest.getInstance("MD5").digest(string.getBytes)
    asString(md5)
  }

  val hexDigits = "0123456789abcdef".toCharArray

  @Override def asString(bytes:Array[Byte]) = {
    bytes.foldLeft(""){case (agg, b) => agg + hexDigits((b >> 4) & 0xf) + hexDigits(b & 0xf)}
  }
}