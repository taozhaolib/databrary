package controllers

import scala.util.control.Exception.catching
import macros._
import dbrary._
import site._

object HTTP {
  def quote(s : String) = '"' + s.replaceAll("([\\p{Cntrl}\"\\\\])", "\\\\$2") + '"'
  def unquote(s : String) =
    if (s.length >= 2 && s.head == '"' && s.last == '"')
      s.tail.init.replaceAll("\\\\(.)", "$1")
    else
      s

  private val rfc1123Date = org.joda.time.format.DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss 'GMT'").withZone(org.joda.time.DateTimeZone.UTC)
  private val rfc850Date  = org.joda.time.format.DateTimeFormat.forPattern("EEEE, dd-MMM-yy HH:mm:ss 'GMT'").withZone(org.joda.time.DateTimeZone.UTC)
  private val asctimeDate = org.joda.time.format.DateTimeFormat.forPattern("EEE MMM d HH:mm:ss yyyy").withZone(org.joda.time.DateTimeZone.UTC)
  def parseDate(s : String) : Option[Timestamp] =
    catching(classOf[IllegalArgumentException]).opt(rfc1123Date.parseLocalDateTime(s)) orElse
    catching(classOf[IllegalArgumentException]).opt(rfc850Date.parseLocalDateTime(s)) orElse
    catching(classOf[IllegalArgumentException]).opt(asctimeDate.parseLocalDateTime(s))
  def date(d : Timestamp) : String =
    rfc1123Date.print(d)
  
  private val rangeRegex = "bytes=([0-9]*)-([0-9]*)".r
  def parseRange(s : String, size : Long) : Option[(Long, Long)] =
    (s match {
      case rangeRegex(start, end) => Maybe.toNumber {
        (Maybe(start).opt.map(_.toLong), Maybe(end).opt.map(_.toLong))
      } getOrElse ((None, None))
      case _ => (None, None)
    }) match {
      case (Some(start), None) => Some((start.min(size), size-1))
      case (Some(start), Some(end)) if start <= end => Some((start.min(size), end.min(size-1)))
      case (None, Some(len)) => Some(((size - len).max(0), size-1))
      case _ => None
    }
}
