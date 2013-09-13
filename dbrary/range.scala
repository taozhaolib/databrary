package dbrary

import java.sql.{SQLException,Date}
import java.util.Calendar

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
  def isEmpty : Boolean = lowerBound.fold(false) { l => upperBound.fold(false) { u =>
    if (lowerClosed && upperClosed) 
      t.gt(l, u)
    else
      t.gteq(l, u)
  } }
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
  def map[B : RangeType](f : A => B) = new Range[B] {
    val lowerBound = self.lowerBound.map(f)
    val upperBound = self.upperBound.map(f)
    val lowerClosed = self.lowerClosed
    val upperClosed = self.upperClosed
  }
}

object Range {
  def empty[A : RangeType] = new Range[A] {
    override val isEmpty = true
    override val singleton = None
    val lowerBound = None
    val upperBound = None
    val lowerClosed = false
    val upperClosed = false
    override def @>(x : A) = false
    override def @>(r : Range[A]) = false
  }
  def singleton[A : RangeType](x : A) = new Range[A] {
    override val isEmpty = false
    override val singleton = Some(x)
    val lowerBound = Some(x)
    val upperBound = Some(x)
    val lowerClosed = true
    val upperClosed = true
  }
  def full[A : RangeType] = new Range[A] {
    override val isEmpty = false
    override val singleton = None
    val lowerBound = None
    val upperBound = None
    val lowerClosed = false
    val upperClosed = false
    override def @>(x : A) = true
    override def @>(r : Range[A]) = true
  }
  def apply[A : RangeType](lb : A, ub : A) = new Range[A] {
    val lowerBound = Some(lb)
    val upperBound = Some(ub)
    val lowerClosed = true
    val upperClosed = true
  }
  def apply[A : RangeType](lc : Boolean, lb : Option[A], ub : Option[A], uc : Boolean) = new Range[A] {
    val lowerBound = lb
    val upperBound = ub
    val lowerClosed = lc
    val upperClosed = uc
  }
}

abstract class PGRangeType[A](val pgType : String)(implicit baseType : PGType[A]) extends RangeType[A] with PGType[Range[A]] {
  def pgGet(s : String) : Range[A] = {
    if (s == "empty" || s.isEmpty)
      return Range.empty[A](this)
    val lc = s.head match {
      case '[' => true
      case '(' => false
      case _ => throw new SQLException("Invalid range: " + s)
    }
    val c = s.indexOf(',', 1)
    if (c < 0)
      throw new SQLException("Invalid range: " + s)
    val lb = if (c == 1) None else Some(baseType.pgGet(s.substring(1,c)))
    val l = s.size
    val ub = if (c == l-2) None else Some(baseType.pgGet(s.substring(c+1,l-1)))
    val uc = s.last match {
      case ']' => true
      case ')' => false
      case _ => throw new SQLException("Invalid range: " + s)
    }
    Range[A](lc, lb, ub, uc)(this)
  }
  def pgPut(r : Range[A]) : String =
    if (r.isEmpty)
      "empty"
    else
      (if (r.lowerClosed) '[' else '(') +
      r.lowerBound.fold("")(baseType.pgPut(_)) + ',' +
      r.upperBound.fold("")(baseType.pgPut(_)) +
      (if (r.upperClosed) ']' else ')')
}

object PGSegment extends PGRangeType[Offset]("segment")(Offset.pgType) {
  def compare(a : Offset, b : Offset) = a compare b
}

object PGDateRange extends PGRangeType[Date]("daterange")(
  new PGType[Date] {
    val pgType = "date"
    def pgGet(s : String) = dbutil.timestampUtils.toDate(null, s)
    def pgPut(d : Date) = dbutil.timestampUtils.toString(null, d)
  }) with DiscreteRangeType[Date] {
  def compare(a : Date, b : Date) = a compareTo b
  private[this] def addDate(a : Date, i : Int) = new Date({ val cal = Calendar.getInstance ; cal.setTime(a) ; cal.add(Calendar.DATE, i) ; cal.getTimeInMillis })
  def increment(a : Date) = addDate(a, +1)
  def decrement(a : Date) = addDate(a, -1)
}
