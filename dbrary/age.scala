package dbrary

import org.joda.time.{Duration,Period,PeriodType}
import play.api.libs.json

case class Age(days : Int) extends scala.math.Ordered[Age] {
  def period : Period = Period.days(days)
  def duration : Duration = Duration.standardDays(days)
  def millis : Long = 86400000L*days // duration.getMillis
  def seconds : Int = 86400*days
  def months : Float = days.toFloat/Age.MONTH;
  def years : Float = days.toFloat/Age.YEAR;
  def compare(that : Age) : Int = days.compare(that.days)
  def min(that : Age) : Age = Age(days.min(that.days))
  def max(that : Age) : Age = Age(days.max(that.days))
}

object Age {
  private final val YEAR : Float = 365.24219f;
  private final val MONTH : Float = YEAR/12;
  def apply(start : Date, end : Date) : Age =
    Age(new Period(start, end, PeriodType.days).getDays)
  implicit object range extends DiscreteRangeType[Age] {
    def compare(a : Age, b : Age) = a.days compare b.days
    def increment(a : Age) = Age(a.days + 1)
    def decrement(a : Age) = Age(a.days - 1)
  }
  def days(d : Int) = Age(d)
  def months(m : Float) = Age((MONTH*m).ceil.toInt);
  def years(y : Float) = Age((YEAR*y).ceil.toInt);
  /** The HIPAA-mandated age cut-off for identifying information.
    * Ages above this should be truncated without appropriate release. */
  final val LIMIT = years(90)

  implicit object jsonFormat extends json.Format[Age] {
    def writes(a : Age) = json.JsNumber(a.days)
    def reads(j : json.JsValue) = json.Json.fromJson[Int](j).map(Age(_))
  }
}
