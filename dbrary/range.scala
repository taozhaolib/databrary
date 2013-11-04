package dbrary

import macros._

trait RangeType[A] extends Ordering[A]
trait DiscreteRangeType[A] extends RangeType[A] {
  def increment(a : A) : A // = a + 1
  def decrement(a : A) : A // = a - 1
}

object RangeType {
  implicit val intRange = new DiscreteRangeType[Int] {
    def increment(a : Int) = a + 1
    def decrement(a : Int) = a - 1
    def compare(a : Int, b : Int) = a compare b
  }
  implicit val longRange = new DiscreteRangeType[Long] {
    def increment(a : Long) = a + 1
    def decrement(a : Long) = a - 1
    def compare(a : Long, b : Long) = a compare b
  }
  implicit val segment : RangeType[Offset] = PGRangeType.segment
}

abstract sealed class Range[A](implicit t : RangeType[A]) {
  self =>
  private[this] def dt = t match {
    case dt : DiscreteRangeType[A] => Some(dt)
    case _ => None
  }
  val lowerBound : Option[A]
  val upperBound : Option[A]
  val lowerClosed : Boolean
  val upperClosed : Boolean
  def lowerPoint : Option[A] = if (lowerClosed) lowerBound else lowerBound.flatMap(lb => dt.map(_.increment(lb)))
  def upperPoint : Option[A] = if (upperClosed) upperBound else upperBound.flatMap(ub => dt.map(_.decrement(ub)))
  def isEmpty : Boolean =
    (for { l <- lowerBound ; u <- upperBound }
     yield {
      if (lowerClosed && upperClosed) 
        t.gt(l, u)
      else
        t.gteq(l, u)
    }).getOrElse(false)
  def isFull : Boolean = lowerBound.isEmpty && upperBound.isEmpty
  def singleton : Option[A] = 
    if (isEmpty)
      None
    else
      upperPoint.flatMap(u => lowerPoint.filter(_ == u))
  def isSingleton : Boolean = singleton.isDefined
  def normalize =
    if (isEmpty || !t.isInstanceOf[DiscreteRangeType[A]])
      self
    else new Range[A] {
      override val isEmpty = false
      val lowerBound = self.lowerPoint
      val upperBound = self.upperPoint
      val lowerClosed = true
      val upperClosed = true
    }
  /* contains relations, as in postgres */
  def @>(x : A) =
    lowerBound.fold(true)(l => if (lowerClosed) t.lteq(l, x) else t.lt(l, x)) &&
    upperBound.fold(true)(u => if (upperClosed) t.lteq(x, u) else t.lt(x, u)) &&
    !isEmpty
  def @>(r : Range[A]) = 
    lowerBound.fold(true)(sl => r.lowerBound.fold(false)(rl => if (lowerClosed >= r.lowerClosed) t.lteq(sl, rl) else t.lt(sl, rl))) &&
    upperBound.fold(true)(su => r.upperBound.fold(false)(ru => if (upperClosed >= r.upperClosed) t.gteq(su, ru) else t.gt(su, ru))) &&
    !isEmpty
  /** Apply a monotonic increasing transform to both end-points.
    * Non-monotonic transforms will result in incoherent ranges. */
  def map[B : RangeType](f : A => B) = new Range[B] {
    override val isEmpty = self.isEmpty
    override val singleton = self.singleton.map(f)
    val lowerBound = self.lowerBound.map(f)
    val upperBound = self.upperBound.map(f)
    val lowerClosed = self.lowerClosed
    val upperClosed = self.upperClosed
  }
}

object Range {
  def empty[A : RangeType] : Range[A] = new Range[A] {
    override val isEmpty = true
    override def isFull = false
    override val singleton = None
    val lowerBound = None
    val upperBound = None
    val lowerClosed = false
    val upperClosed = false
    override def @>(x : A) = false
    override def @>(r : Range[A]) = false
  }
  def singleton[A : RangeType](x : A) : Range[A] = new Range[A] {
    override val isEmpty = false
    override def isFull = false
    override val singleton = Some(x)
    val lowerBound = Some(x)
    val upperBound = Some(x)
    val lowerClosed = true
    val upperClosed = true
  }
  def full[A : RangeType] : Range[A] = new Range[A] {
    override val isEmpty = false
    override def isFull = true
    override val singleton = None
    val lowerBound = None
    val upperBound = None
    val lowerClosed = false
    val upperClosed = false
    override def @>(x : A) = true
    override def @>(r : Range[A]) = true
  }
  def apply[A : RangeType](lb : A, ub : A) : Range[A] = new Range[A] {
    override def isFull = false
    val lowerBound = Some(lb)
    val upperBound = Some(ub)
    val lowerClosed = true
    val upperClosed = implicitly[RangeType[A]].isInstanceOf[DiscreteRangeType[A]] || lb == ub
  }
  def apply[A : RangeType](lb : Option[A], ub : Option[A]) : Range[A] = new Range[A] {
    val lowerBound = lb
    val upperBound = ub
    val lowerClosed = lb.isDefined
    val upperClosed = ub.fold(false)(_ => implicitly[RangeType[A]].isInstanceOf[DiscreteRangeType[A]] || lb == ub)
  }
  def apply[A : RangeType](lc : Boolean, lb : Option[A], ub : Option[A], uc : Boolean) : Range[A] = new Range[A] {
    val lowerBound = lb
    val upperBound = ub
    val lowerClosed = lc
    val upperClosed = uc
  }

  implicit val segmentSqlType : SQLType[Range[Offset]] = PGRangeType.segment.sqlType
}

abstract class PGRangeType[A](name : String)(implicit base : SQLType[A]) extends RangeType[A] {
  implicit val sqlType = SQLType[Range[A]](name, classOf[Range[A]])({ s =>
    if (s.equals("empty") || s.isEmpty)
      Some(Range.empty[A](this))
    else for {
      lc <- s.head match {
        case '[' => Some(true)
        case '(' => Some(false)
        case _ => None
      }
      c <- Maybe(s.indexOf(',', 1)).opt
      lb <- if (c == 1) Some(None) else base.read(s.substring(1,c)).map(Some(_))
      l = s.size
      ub <- if (c == l-2) Some(None) else base.read(s.substring(c+1,l-1)).map(Some(_))
      uc <- s.last match {
        case ']' => Some(true)
        case ')' => Some(false)
        case _ => None
      }
    } yield (Range[A](lc, lb, ub, uc)(this))
  }, { r =>
    if (r.isEmpty)
      "empty"
    else
      (if (r.lowerClosed) '[' else '(') +
      r.lowerBound.fold("")(base.show(_)) + ',' +
      r.upperBound.fold("")(base.show(_)) +
      (if (r.upperClosed) ']' else ')')
  })
}

object PGRangeType {
  implicit object segment extends PGRangeType[Offset]("segment") {
    def compare(a : Offset, b : Offset) = a compare b
  }

  implicit object daterange extends PGRangeType[Date]("daterange") with DiscreteRangeType[Date] {
    def compare(a : Date, b : Date) = a compareTo b
    def increment(a : Date) = a.plusDays(1)
    def decrement(a : Date) = a.minusDays(1)
  }
}
