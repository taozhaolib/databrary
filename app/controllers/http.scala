package controllers

import scala.util.control.Exception.catching
import macros._
import site._

object HTTP {
  def quote(s : String) = '"' + s.replaceAll("([\\p{Cntrl}\"\\\\])", "\\\\$2") + '"'
  def unquote(s : String) =
    if (s.length >= 2 && s.head == '"' && s.last == '"')
      s.tail.init.replaceAll("\\\\(.)", "$1")
    else
      s

  private val tzGMT = java.util.TimeZone.getTimeZone("GMT")
  private val rfc1123Date = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss 'GMT'")
  rfc1123Date.setTimeZone(tzGMT)
  private val rfc850Date = new java.text.SimpleDateFormat("EEEE, dd-MMM-yy HH:mm:ss 'GMT'")
  rfc850Date.setTimeZone(tzGMT)
  private val asctimeDate = new java.text.SimpleDateFormat("EEE MMM d HH:mm:ss yyyy")
  asctimeDate.setTimeZone(tzGMT)
  def parseDate(s : String) : Option[java.util.Date] = 
    catching(classOf[java.text.ParseException]).opt(rfc1123Date.parse(s)) orElse
    catching(classOf[java.text.ParseException]).opt(rfc850Date.parse(s)) orElse
    catching(classOf[java.text.ParseException]).opt(asctimeDate.parse(s))
  
  private val rangeRegex = "bytes=([0-9]*)-([0-9]*)".r
  def parseRange(s : String, size : Long) : Option[(Long, Long)] =
    (s match {
      case rangeRegex(start, end) => catching(classOf[java.lang.NumberFormatException]).opt {
        (Maybe.opt(start).map(_.toLong), Maybe.opt(end).map(_.toLong))
      } getOrElse ((None, None))
      case _ => (None, None)
    }) match {
      case (Some(start), None) => Some((start.min(size), size-1))
      case (Some(start), Some(end)) if start <= end => Some((start.min(size), end.min(size-1)))
      case (None, Some(len)) => Some(((size - len).max(0), size-1))
      case _ => None
    }
}
