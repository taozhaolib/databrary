package dbrary

import org.joda.time.{Duration,Period,PeriodType}

case class Age(days : Int) {
  def period : Period = Period.days(days)
  def duration : Duration = Duration.standardDays(days)
  def millis : Long = 86400000L*days // duration.getMillis
  def min(that : Age) : Age = Age(days.min(that.days))
  def max(that : Age) : Age = Age(days.max(that.days))
}

object Age {
  def apply(start : Date, end : Date) : Age =
    Age(new Period(start, end, PeriodType.days).getDays)
  implicit val range = new DiscreteRangeType[Age] {
    def compare(a : Age, b : Age) = a.days compare b.days
    def increment(a : Age) = Age(a.days + 1)
    def decrement(a : Age) = Age(a.days - 1)
  }
}
