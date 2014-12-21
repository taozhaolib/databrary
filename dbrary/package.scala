package object dbrary {
  type Segment = Range[Offset]
  type Section = BoundedRange[Offset]

  import org.joda.time
  /* All times are assumed to be UTC. */
  type Date = time.LocalDate
  type Timestamp = time.LocalDateTime

  def init() {
    time.DateTimeZone.setDefault(time.DateTimeZone.UTC)
  }

  import play.api.libs.json
  implicit object timestampJson extends json.Format[Timestamp] {
    def writes(t : Timestamp) = json.JsNumber(t./*getLocalMillis*/toDateTime.getMillis)
    def reads(j : json.JsValue) = j.validate[Long].map(new Timestamp(_))
  }

  implicit def urlFormatter : play.api.data.format.Formatter[java.net.URL] = url.formatter
  implicit def urlJson : json.Format[java.net.URL] = url.jsonFormat
}
