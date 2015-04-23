package controllers

import scala.util.control.Exception.catching
import play.api.http.HeaderNames
import play.api.mvc._
import macros._
import dbrary._
import site._

object HTTP extends HeaderNames {
  def quote(s : String) = '"' + s.replaceAll("""([\p{Cntrl}"\\])""", "\\\\$1") + '"'
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
        (Maybe(start).opt(_.toLong), Maybe(end).opt(_.toLong))
      } getOrElse ((None, None))
      case _ => (None, None)
    }) match {
      case (Some(start), None) => Some((start.min(size), size-1))
      case (Some(start), Some(end)) if start <= end => Some((start.min(size), end.min(size-1)))
      case (None, Some(len)) => Some(((size - len).max(0), size-1))
      case _ => None
    }

  def notModified(etag : String, date : Timestamp)(implicit request : Request[_]) : Boolean = {
    /* The split works because we never use commas within etags. */
    val ifNoneMatch = request.headers.getAll(IF_NONE_MATCH).flatMap(_.split(',').map(_.trim))
    ifNoneMatch.exists(t => t.equals("*") || unquote(t).equals(etag)) ||
      ifNoneMatch.isEmpty && request.headers.get(IF_MODIFIED_SINCE).flatMap(parseDate).exists(!date.isAfter(_))
  }

  def wsResult(ws : play.api.libs.ws.WSResponse) : Result =
    Result(new ResponseHeader(ws.status, ws.allHeaders.mapValues(_.head)), play.api.libs.iteratee.Enumerator(ws.body.getBytes))
}
