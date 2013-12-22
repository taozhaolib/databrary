package object dbrary {
  import org.joda.time
  /* All times are assumed to be UTC. */
  type Date = time.LocalDate
  type Timestamp = time.LocalDateTime

  time.DateTimeZone.setDefault(time.DateTimeZone.UTC)

  import play.api.libs.json
  implicit val timestampJson : json.Format[Timestamp] = new json.Format[Timestamp] {
    def writes(t : Timestamp) = json.JsNumber(t./*getLocalMillis*/toDateTime.getMillis)
    def reads(j : json.JsValue) = j match {
      case json.JsNumber(t) if t.isValidLong => json.JsSuccess(new Timestamp(t.toLong))
      case _ => json.JsError("error.expected.jsnumber")
    }
  }
}
