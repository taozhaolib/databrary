package dbrary

import play.api.mvc.{PathBindable,QueryStringBindable,JavascriptLitteral}
import play.api.data.format.Formatter
import play.api.libs.json
import org.postgresql.util.PGInterval
import macros._

/* A length of time; called "interval" in postgres, offset is a better name for our purposes */
case class Offset(seconds : Double) 
  extends scala.math.Ordered[Offset] {
  /*
     with scala.runtime.FractionalProxy[Double] 
     with scala.runtime.OrderedProxy[Double] {
     def self = seconds
  protected def ord = scala.math.Ordering.Double
  protected def integralNum = scala.math.Numeric.DoubleAsIfIntegral
  protected def num = scala.math.Numeric.DoubleIsFractional
  */
  def millis = 1000*seconds
  def nanos = 1000000000*seconds
  def samples(rate : Double) = math.round(rate*seconds)
  def +(other : Offset) = Offset(seconds + other.seconds)
  def -(other : Offset) = Offset(seconds - other.seconds)
  def compare(other : Offset) : Int = seconds.compare(other.seconds)
  def min(other : Offset) = Offset(seconds.min(other.seconds))
  def max(other : Offset) = Offset(seconds.max(other.seconds))
  def approx(other : Offset) = (other.seconds - seconds).abs < 0.001

  /* This is unfortunate but I can't find any other reasonable formatting options outside the postgres server itself: */
  override def toString = {
    val secs = seconds.abs
    val s = "%06.3f".format(secs % 60)
    val m = secs.toInt / 60
    (if (seconds.signum < 0) "-" else "") +
    (if (m >= 60)
      "%02d:%02d:%s".format(m / 60, m % 60, s)
    else
      "%02d:%s".format(m, s))
  }
}

object Offset {
  def apply(d : BigDecimal) : Offset = Offset(d.toDouble)
  def apply(i : PGInterval) : Offset =
    Offset(60*(60*(24*(30*(12.175*i.getYears + i.getMonths) + i.getDays) + i.getHours) + i.getMinutes) + i.getSeconds)

  private val multipliers : Seq[Double] = Seq(60,60,24).scanLeft(1.0)(_ * _)
  def fromString(s : String) : Offset =
    s.stripPrefix("-").split(':').reverseIterator.zipAll(multipliers.iterator, "", 0.0).map {
      case (_, 0) => throw new java.lang.NumberFormatException("For offset string: " + s)
      case ("", _) => 0.0
      case (s, m) => m*s.toDouble
    }.sum * (if (s.startsWith("-")) -1.0 else 1.0)

  implicit val sqlType : SQLType[Offset] =
    SQLType[Offset]("interval", classOf[Offset])(
      /* FIXME: > 1 day. see https://github.com/mauricio/postgresql-async/pull/56 for a fix */
      s => Maybe.toNumber(fromString(s)),
      _.seconds.toString
    )

  implicit val pathBindable : PathBindable[Offset] = PathBindable.bindableDouble.transform(apply _, _.seconds)
  implicit val queryStringBindable : QueryStringBindable[Offset] = QueryStringBindable.bindableDouble.transform(apply _, _.seconds)
  implicit val javascriptLitteral : JavascriptLitteral[Offset] = new JavascriptLitteral[Offset] {
    def to(value : Offset) = value.seconds.toString
  }

  implicit val offsetFormat : Formatter[Offset] = new Formatter[Offset] {
    override val format = Some(("format.offset", Nil))
    def bind(key: String, data: Map[String, String]) =
      data.get(key).flatMap(s => Maybe.toNumber(fromString(s)))
        .toRight(Seq(play.api.data.FormError(key, "error.offset", Nil)))
    def unbind(key: String, value: Offset) = Map(key -> value.toString)
  }

  implicit val jsonFormat : json.Format[Offset] = new json.Format[Offset] {
    def writes(o : Offset) = json.JsNumber(o.seconds)
    def reads(j : json.JsValue) = j match {
      case json.JsNumber(s) => json.JsSuccess(apply(s))
      case _ => json.JsError("error.expected.jsnumber")
    }
  }

  import scala.language.implicitConversions
  implicit def ofSeconds(seconds : Double) : Offset = Offset(seconds)
}

