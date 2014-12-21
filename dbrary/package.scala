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

  private[dbrary] val logger : play.api.Logger = play.api.Logger("sql")
  private[dbrary] def quoted(s : String) =
    "'" + s.replaceAllLiterally("'", "''") + "'";

  import play.api.libs.json
  implicit object timestampJson extends json.Format[Timestamp] {
    def writes(t : Timestamp) = json.JsNumber(t./*getLocalMillis*/toDateTime.getMillis)
    def reads(j : json.JsValue) = j.validate[Long].map(new Timestamp(_))
  }

  implicit def urlFormatter : play.api.data.format.Formatter[java.net.URL] = url.formatter
  implicit def urlJson : json.Format[java.net.URL] = url.jsonFormat

  implicit final class SQLInterpolator(private val sc: StringContext) {
    object sql extends SQLArgsView[PreparedStatement] {
      protected def result(args : SQLArg[_]*) =
        new PreparedStatement(sc.s(args.map(_.placeholder) : _*), args)
    }
    object lsql extends SQLArgsView[LiteralStatement] {
      protected def result(args : SQLArg[_]*) =
        new LiteralStatement(sc.s(args.map(_.escaped) : _*))
    }
  }
}
