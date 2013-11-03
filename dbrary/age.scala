package dbrary

import org.joda.time.{Duration,Period,PeriodType}

case class Age(days : Int) {
  lazy val period : Period = Period.days(days)
  lazy val duration : Duration = Duration.standardDays(days)
  lazy val millis : Long = 86400000L*days // duration.getMillis
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
