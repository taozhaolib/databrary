package dbrary

import java.sql.SQLException

trait RangeType[A] {
  protected val unit : Option[A]
  def discrete = unit.isDefined
  def increment(a : A) : Option[A] // = unit.map(a + _)
  def decrement(a : A) : Option[A] // = unit.map(a - _)
}

abstract sealed class Range[A <: Ordered[A]](implicit t : RangeType[A]) {
  self =>
  val lowerBound : Option[A]
  val upperBound : Option[A]
  val lowerClosed : Boolean
  val upperClosed : Boolean
  def lowerPoint : Option[A] = if (lowerClosed) lowerBound else lowerBound.flatMap(t.increment(_))
  def upperPoint : Option[A] = if (upperClosed) upperBound else upperBound.flatMap(t.decrement(_))
  def isEmpty : Boolean = lowerBound.fold(false) { l => upperBound.fold(false) { u =>
    if (lowerClosed && upperClosed) 
      l > u
    else
      l >= u
  } }
  def singleton : Option[A] = 
    if (isEmpty)
      None
    else
      upperPoint.flatMap(u => lowerPoint.filter(_ == u))
  def isSingleton : Boolean = singleton.isDefined
  def normalize =
    if (isEmpty || !t.discrete)
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
    lowerBound.fold(true)(l => if (lowerClosed) l <= x else l < x) &&
    upperBound.fold(true)(u => if (upperClosed) x <= u else x < u) &&
    !isEmpty
  def @>(r : Range[A]) = 
    lowerBound.fold(true)(sl => r.lowerBound.fold(false)(rl => if (lowerClosed >= r.lowerClosed) sl <= rl else sl < rl)) &&
    upperBound.fold(true)(su => r.upperBound.fold(false)(ru => if (upperClosed >= r.upperClosed) su >= ru else su > ru)) &&
    !isEmpty
}

object Range {
  def empty[A <: Ordered[A]](implicit t : RangeType[A]) = new Range[A] {
    override val isEmpty = true
    override val singleton = None
    val lowerBound = None
    val upperBound = None
    val lowerClosed = false
    val upperClosed = false
    override def @>(x : A) = false
    override def @>(r : Range[A]) = false
  }
  def singleton[A <: Ordered[A]](x : A)(implicit t : RangeType[A]) = new Range[A] {
    override val isEmpty = false
    override val singleton = Some(x)
    val lowerBound = Some(x)
    val upperBound = Some(x)
    val lowerClosed = true
    val upperClosed = true
  }
  def apply[A <: Ordered[A]](lb : A, ub : A)(implicit t : RangeType[A]) = new Range[A] {
    val lowerBound = Some(lb)
    val upperBound = Some(ub)
    val lowerClosed = true
    val upperClosed = true
  }
  def apply[A <: Ordered[A]](lc : Boolean, lb : Option[A], ub : Option[A], uc : Boolean)(implicit t : RangeType[A]) = new Range[A] {
    val lowerBound = lb
    val upperBound = ub
    val lowerClosed = lc
    val upperClosed = uc
  }
}

abstract class PGRangeType[A <: Ordered[A]](val pgType : String)(implicit baseType : PGType[A]) extends RangeType[A] with PGType[Range[A]] {
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
  val unit = None
  def increment(a : Offset) = None
  def decrement(a : Offset) = None
}
