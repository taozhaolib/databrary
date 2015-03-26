package dbrary

import scala.util.control.Exception.catching
import play.api.mvc.{PathBindable,QueryStringBindable,JavascriptLitteral}
import play.api.data.format.Formatter
import play.api.libs.json
import org.postgresql.util.PGInterval
import org.joda.time
import macros._

/* A length of time.
 * Called "interval" in postgres and Duration in joda, offset is a better name for our purposes. */
final case class Offset(millis : Long) extends scala.math.Ordered[Offset] {
  // def nanos = 1000000L*seconds
  override def hashCode = millis.hashCode
  override def equals(o : Any) = o match {
    case Offset(m) => millis == m
    case _ => false
  }
  def ==(o : Offset) = millis == o.millis
  def seconds : Double = millis/1000.0
  def samples(rate : Double) = math.round(rate*seconds)
  def +(other : Offset) = new Offset(millis + other.millis)
  def -(other : Offset) = new Offset(millis - other.millis)
  def unary_- = new Offset(-millis)
  def compare(other : Offset) : Int = millis.compare(other.millis)
  def min(other : Offset) = new Offset(millis.min(other.millis))
  def max(other : Offset) = new Offset(millis.max(other.millis))
  def duration : time.Duration = new time.Duration(millis)
  def abs = new Offset(millis.abs)

  /* This is unfortunate but I can't find any other reasonable formatting options without the postgres server or converting to a joda Period */
  override def toString = {
    val ms = millis.abs
    val m = ms / 60000
    (if (millis.signum < 0) "-" else "") +
    (if (m < 60) m.formatted("%02d:")
     else "%02d:%02d:".format(m / 60, m % 60)) +
    ((ms % 60000)/1000.0).formatted("%06.3f")
  }
}

object Offset {
  final val ZERO = new Offset(0)
  final val SECOND = ofSeconds(1)
  // def apply(d : BigDecimal) : Offset = new Offset((1000L*d).toLong)
  def apply(i : PGInterval) : Offset =
    ofSeconds(60*(60*(24*(30*(12.175*i.getYears + i.getMonths) + i.getDays) + i.getHours) + i.getMinutes) + i.getSeconds)
  def apply(d : time.Duration) : Offset = new Offset(d.getMillis)
  def ofSeconds(seconds : Double) : Offset =
    if (seconds.isInfinite /* why !seconds.isValidLong? */) throw new java.lang.NumberFormatException("Invalid offset")
    else new Offset((1000.0*seconds).toLong)

  trait numeric extends Numeric[Offset] {
    private val long = Numeric.LongIsIntegral
    def compare(x : Offset, y : Offset) = long.compare(x.millis, y.millis)
    def fromInt(x : Int) = new Offset(1000*x)
    def plus(x : Offset, y : Offset) = new Offset(long.plus(x.millis, y.millis))
    def minus(x : Offset, y : Offset) = new Offset(long.minus(x.millis, y.millis))
    def negate(x : Offset) = new Offset(long.negate(x.millis))
    def times(x : Offset, y : Offset) = new Offset(long.times(x.millis, y.millis) / 1000)
    def toDouble(x : Offset) = x.millis / 1000.0
    def toFloat(x : Offset) = x.millis / 1000.0f
    def toInt(x : Offset) = (x.millis / 1000).toInt
    def toLong(x : Offset) = x.millis / 1000
  }

  private val multipliers : Seq[Double] = Seq(60,60,24).scanLeft(1.0)(_ * _)
  def fromString(s : String) : Offset = Maybe.toLong(s).fold(
    ofSeconds(s.stripPrefix("-").split(':').reverseIterator.zipAll(multipliers.iterator, "", 0.0).map {
      case (_, 0) => throw new java.lang.NumberFormatException("For offset string: " + s)
      case ("", _) => 0.0
      case (s, m) => m*s.toDouble
    }.sum * (if (s.startsWith("-")) -1.0 else 1.0)))(
    new Offset(_))

  implicit val sqlType : SQL.Type[Offset] =
    SQL.Type.interval.transform[Offset]("interval", classOf[Offset])(
      p => catching(classOf[UnsupportedOperationException])
        .opt(new Offset(p.toStandardDuration.getMillis)),
      o => new time.Period(o.millis)
    )

  implicit val pathBindable : PathBindable[Offset] = PathBindable.bindableLong.transform(new Offset(_), _.millis)
  implicit object queryStringBindable extends QueryStringBindable[Offset] {
    def bind(key : String, params : Map[String, Seq[String]]) : Option[Either[String, Offset]] =
      params.get(key).flatMap(_.headOption).map { s =>
        Maybe.toNumber(fromString(s))
        .toRight("invalid offset parameter value for " + key)
      }
    def unbind(key : String, offset : Offset) : String =
      QueryStringBindable.bindableLong.unbind(key, offset.millis)
  }
  implicit object javascriptLitteral extends JavascriptLitteral[Offset] {
    def to(value : Offset) = value.millis.toString
  }

  implicit object formatter extends Formatter[Offset] {
    override val format = Some(("format.offset", Nil))
    def bind(key: String, data: Map[String, String]) =
      data.get(key).flatMap(s => 
        Maybe.toNumber(fromString(s)))
        .toRight(Seq(play.api.data.FormError(key, "error.offset", Nil)))
    def unbind(key: String, value: Offset) = Map(key -> value.toString)
  }

  implicit object jsonFormat extends json.Format[Offset] {
    private final val err = json.JsError(Seq(json.JsPath() -> Seq(play.api.data.validation.ValidationError("error.offset"))))
    def writes(o : Offset) = json.JsNumber(o.millis)
    def reads(j : json.JsValue) = j match {
      case json.JsNumber(o) => json.JsSuccess(new Offset(o.toLong))
      case json.JsString(s) => Maybe.toNumber(fromString(s)).fold[json.JsResult[Offset]](err)(json.JsSuccess(_))
      case _ => err
    }
  }
}

