package dbrary

import play.api.mvc.{PathBindable,QueryStringBindable,JavascriptLitteral}
import play.api.data.format.Formatter
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
  def compare(other : Offset) = seconds.compare(other.seconds)
  def min(other : Offset) = Offset(seconds.min(other.seconds))
  def max(other : Offset) = Offset(seconds.max(other.seconds))
  def approx(other : Offset) = (other.seconds - seconds).abs < 0.001

  /* This is unfortuante but I can't find any other reasonable formatting options outside the postgres server itself: */
  override def toString = {
    val s = "%06.3f".format(seconds % 60)
    val m = seconds.toInt / 60
    if (m >= 60)
      "%02d:%02d:%s".format(m / 60, m % 60, s)
    else
      "%02d:%s".format(m, s)
  }
}

object Offset {
  def apply(d : BigDecimal) : Offset = Offset(d.toDouble)
  def apply(i : PGInterval) : Offset =
    Offset(60*(60*(24*(30*(12.175*i.getYears + i.getMonths) + i.getDays) + i.getHours) + i.getMinutes) + i.getSeconds)

  private val multipliers : Seq[Double] = Seq(60,60,24).scanLeft(1.)(_ * _)
  def fromString(s : String) : Offset = {
    s.split(':').reverseIterator.zipAll(multipliers.iterator, "", 0.).map {
      case (_, 0) => throw new java.lang.NumberFormatException("For offset string: " + s)
      case ("", _) => 0.
      case (s, m) => m*s.toDouble
    }.sum
  }

  implicit val sqlType : SQLType[Offset] =
    SQLType.mapping[String,Offset]("interval", classOf[Offset]) { s =>
      /* FIXME: > 1 day. see https://github.com/mauricio/postgresql-async/pull/56 for a fix */
      maybe.toNumber(fromString(s))
    } { i =>
      i.seconds.toString
    }

  implicit val pathBindable : PathBindable[Offset] = PathBindable.bindableDouble.transform(apply _, _.seconds)
  implicit val queryStringBindable : QueryStringBindable[Offset] = QueryStringBindable.bindableDouble.transform(apply _, _.seconds)
  implicit val javascriptLitteral : JavascriptLitteral[Offset] = new JavascriptLitteral[Offset] {
    def to(value : Offset) = value.seconds.toString
  }

  implicit val offsetFormat : Formatter[Offset] = new Formatter[Offset] {
    override val format = Some(("format.offset", Nil))
    def bind(key: String, data: Map[String, String]) =
      play.api.data.format.Formats.stringFormat.bind(key, data).right.flatMap { s =>
        scala.util.control.Exception.catching(classOf[java.lang.NumberFormatException]).
          either(fromString(s)).
          left.map(_ => Seq(play.api.data.FormError(key, "error.offset", Nil)))
      }
    def unbind(key: String, value: Offset) = Map(key -> value.toString)
  }

  import scala.language.implicitConversions
  implicit def ofSeconds(seconds : Double) : Offset = Offset(seconds)
}

