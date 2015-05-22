package dbrary

import play.api.libs.json
import play.api.mvc.QueryStringBindable
import play.api.data.format.Formatter
import play.api.data.format.Formats.booleanFormat
import play.api.data.FormError
import macros._

trait RangeType[A] extends Ordering[A] {
  t =>
  def isDiscrete = false

  object SomeOrdering extends PartialOrdering[Option[A]] {
    def tryCompare(x : Option[A], y : Option[A]) : Option[Int] =
      zip(x, y, t.compare _)
    override def equiv(x : Option[A], y : Option[A]) : Boolean =
      zip(x, y, t.equiv _).getOrElse(false)
    def lteq(x : Option[A], y : Option[A]) : Boolean =
      zip(x, y, t.lteq _).getOrElse(false)
    override def gteq(x : Option[A], y : Option[A]) : Boolean =
      zip(x, y, t.gteq _).getOrElse(false)
    override def lt(x : Option[A], y : Option[A]) : Boolean =
      zip(x, y, t.lt _).getOrElse(false)
    override def gt(x : Option[A], y : Option[A]) : Boolean =
      zip(x, y, t.gt _).getOrElse(false)
  }
  object LowerOrdering extends Ordering[Range[A]] {
    def compare(x : Range[A], y : Range[A]) : Int = {
      val xe = x.isEmpty
      val ye = y.isEmpty
      if (xe || ye) xe compare ye
      else {
        val xb = x.lowerBound
        val yb = y.lowerBound
        (for { xb <- xb ; yb <- yb } yield {
          val c = t.compare(xb, yb)
          if (c != 0) c else y.lowerClosed compare x.lowerClosed
        }).getOrElse(xb.isDefined compare yb.isDefined)
      }
    }
  }
  object UpperOrdering extends Ordering[Range[A]] {
    def compare(x : Range[A], y : Range[A]) : Int = {
      val xe = x.isEmpty
      val ye = y.isEmpty
      if (xe || ye) ye compare xe
      else {
        val xb = x.upperBound
        val yb = y.upperBound
        (for { xb <- xb ; yb <- yb } yield {
          val c = t.compare(xb, yb)
          if (c != 0) c else x.lowerClosed compare y.lowerClosed
        }).getOrElse(xb.isEmpty compare yb.isEmpty)
      }
    }
  }
  object TotalOrdering extends Ordering[Range[A]] {
    def compare(x : Range[A], y : Range[A]) : Int = {
      val o = LowerOrdering.compare(x, y)
      if (o != 0) o else UpperOrdering.compare(x, y)
    }
  }
  object Nesting extends PartialOrdering[Range[A]] {
    def tryCompare(x : Range[A], y : Range[A]) : Option[Int] = {
      val lo = LowerOrdering.compare(x, y)
      val uo = UpperOrdering.compare(x, y)
      if (lo == 0 && uo == 0) Some(0)
      else if (lo <= 0 && uo >= 0) Some(1)
      else if (lo >= 0 && uo <= 0) Some(-1)
      else None
    }
    def lteq(x : Range[A], y : Range[A]) =
      tryCompare(x, y).exists(_ <= 0)
  }
}

trait DiscreteRangeType[A] extends RangeType[A] {
  def increment(a : A) : A // = a + 1
  def decrement(a : A) : A // = a - 1
  override final def isDiscrete = true
}

trait NumericRangeType[A] extends RangeType[A] with Numeric[A] {
  object SizeOrdering extends Ordering[Range[A]] {
    def compare(x : Range[A], y : Range[A]) : Int = ???
  }
}

object RangeType {
  implicit val segment : NumericRangeType[Offset] = PGRangeType.segment
  implicit val date : DiscreteRangeType[Date] = PGRangeType.date
}

abstract sealed class Range[A](implicit t : RangeType[A]) {
  self =>
  private[this] def dt = cast[DiscreteRangeType[A]](t)
  val lowerBound : Option[A]
  val upperBound : Option[A]
  val lowerClosed : Boolean
  val upperClosed : Boolean
  final def lowerPoint : Option[A] = if (lowerClosed) lowerBound else lowerBound.flatMap(lb => dt.map(_.increment(lb)))
  final def upperPoint : Option[A] = if (upperClosed) upperBound else upperBound.flatMap(ub => dt.map(_.decrement(ub)))
  def isEmpty : Boolean =
    zip { (l, u) =>
      if (lowerClosed && upperClosed) 
        t.gt(l, u)
      else
        t.gteq(l, u)
    }.getOrElse(false)
  def isFull : Boolean = lowerBound.isEmpty && upperBound.isEmpty
  def singleton : Option[A] = 
    upperPoint.flatMap(u => lowerPoint.filter(t.equiv(_, u)))
  def isSingleton : Boolean = singleton.isDefined
  /** Is this a canonical representation of this range, meaning EmptyRange, FullRange, SingletonRange, [x,y] for discrete types, or [x,y) for continuous types. */
  def isNormalized = false
  final def normalize = if (isNormalized) self else
    singleton.fold {
      if (isEmpty) new EmptyRange[A]
      else if (isFull) new FullRange[A]
      else if (t.isDiscrete) new NormalRange[A](lowerPoint, upperPoint)
      else if (lowerClosed == lowerBound.isDefined && upperClosed == t.SomeOrdering.equiv(lowerBound, upperBound)) new NormalRange[A](lowerBound, upperBound)
      else self
    } (new SingletonRange[A](_))
  final override def hashCode = if (isEmpty) 0 else (lowerBound, upperBound).hashCode
  final def canEqual(o : Any) = o.isInstanceOf[Range[A]]
  final override def equals(o : Any) = o match {
    case r : Range[_] if r.isEmpty => r.canEqual(this) && isEmpty
    case r : Range[_] => r.canEqual(this) && !isEmpty && lowerBound.equals(r.lowerBound) && upperBound.equals(r.upperBound)
    case _ => false
  }
  /** Contains relations, as in postgres. */
  final def @>(x : A) = isFull || !isEmpty &&
    lowerBound.forall(l => if (lowerClosed) t.lteq(l, x) else t.lt(l, x)) &&
    upperBound.forall(u => if (upperClosed) t.lteq(x, u) else t.lt(x, u))
  final def @>(r : Range[A]) = t.Nesting.lteq(r, this)
  /** Intersection, as in postgres. */
  final def *(r : Range[A]) =
    if (isEmpty || r.isFull) this
    else if (isFull || r.isEmpty) r
    else {
      val l = t.LowerOrdering.max(this, r)
      val u = t.UpperOrdering.min(this, r)
      Range[A](l.lowerClosed, l.lowerBound, u.upperBound, u.upperClosed)
    }
  /** Apply a monotonic increasing transform to both end-points.
    * Non-monotonic transforms will result in incoherent ranges. */
  final def map[B : RangeType](f : A => B) = new Range[B] {
    override val isEmpty = self.isEmpty
    override val singleton = self.singleton.map(f)
    val lowerBound = self.lowerBound.map(f)
    val upperBound = self.upperBound.map(f)
    val lowerClosed = self.lowerClosed
    val upperClosed = self.upperClosed
  }
  final def zip[B](f : (A, A) => B) : Option[B] =
    for {
      l <- lowerBound
      u <- upperBound
    } yield (f(l,u))
  final def ===(a : Range[A]) = {
    val oo = Ordering.Option(t)
    oo.equiv(lowerBound, a.lowerBound) &&
    oo.equiv(upperBound, a.upperBound) &&
    lowerClosed == a.lowerClosed &&
    upperClosed == a.upperClosed &&
    isEmpty == a.isEmpty
  }
  override def toString =
    (if (lowerClosed) "[" else "(") +
    lowerBound.fold("")(_.toString) +
    "," +
    upperBound.fold("")(_.toString) +
    (if (upperClosed) "]" else ")")
}

final class EmptyRange[A : RangeType] extends Range[A] {
  override def isEmpty = true
  override def isFull = false
  override def singleton = None
  val lowerBound = None
  val upperBound = None
  val lowerClosed = false
  val upperClosed = false
  override def isNormalized = true
  override def toString = "empty"
}

final class FullRange[A : RangeType] extends Range[A] {
  override def isEmpty = false
  override def isFull = true
  override def singleton = None
  val lowerBound = None
  val upperBound = None
  val lowerClosed = false
  val upperClosed = false
  override def isNormalized = true
}

class NormalRange[A](lb : Option[A], ub : Option[A])(implicit t : RangeType[A]) extends Range[A] {
  final val lowerBound = lb
  final val upperBound = ub
  val lowerClosed = lb.isDefined
  val upperClosed = ub.isDefined && (t.isDiscrete || t.SomeOrdering.equiv(lb, ub))
  override def isEmpty = t.SomeOrdering.gt(lb, ub)
  final override def isNormalized = !isEmpty
}

case class BoundedRange[A](lower : A, upper : A)(implicit t : RangeType[A]) extends NormalRange[A](Some(lower), Some(upper)) {
  override val lowerClosed = true
  override val upperClosed = t.isDiscrete || isSingleton
  override def isFull = false
  override def isEmpty = t.gt(lower, upper)
  override def isSingleton = t.equiv(lower, upper)
  override def singleton = if (isSingleton) Some(lower) else None
}

final class SingletonRange[A : RangeType](x : A) extends BoundedRange[A](x, x) {
  override val upperClosed = true
  override def isEmpty = false
  override def isSingleton = true
  override def singleton = Some(x)
}

object Range {
  def empty[A : RangeType] : EmptyRange[A] =
    new EmptyRange[A]
  def singleton[A : RangeType](x : A) : SingletonRange[A] =
    new SingletonRange[A](x)
  def full[A : RangeType] : FullRange[A] =
    new FullRange[A]
  def apply[A : RangeType](lb : A, ub : A) : BoundedRange[A] =
    new BoundedRange[A](lb, ub)
  def apply[A](lb : Option[A], ub : Option[A])(implicit t : RangeType[A]) : NormalRange[A] =
    new NormalRange[A](lb, ub)
  def apply[A : RangeType](lc : Boolean, lb : Option[A], ub : Option[A], uc : Boolean) : Range[A] = new Range[A] {
    val lowerBound = lb
    val upperBound = ub
    val lowerClosed = lc
    val upperClosed = uc
  }
  def size[A](r : Range[A])(implicit t : NumericRangeType[A]) : Option[A] =
    if (r.isEmpty) Some(t.zero) else r.zip((l,u) => t.minus(u, l))

  def lowerOrdering[A](implicit t : RangeType[A]) : Ordering[Range[A]] = t.LowerOrdering
  def upperOrdering[A](implicit t : RangeType[A]) : Ordering[Range[A]] = t.UpperOrdering
  def totalOrdering[A](implicit t : RangeType[A]) : Ordering[Range[A]] = t.TotalOrdering
  def nesting[A](implicit t : RangeType[A]) : PartialOrdering[Range[A]] = t.Nesting
  implicit val segmentSqlType : SQL.Type[Range[Offset]] = PGRangeType.segment.sqlType
  implicit val dateSqlType : SQL.Type[Range[Date]] = PGRangeType.date.sqlType
  implicit def jsonWrites[T : json.Writes] : json.Writes[Range[T]] =
    json.Writes[Range[T]] { o =>
      if (o.isEmpty) json.JsNull
      else if (o.isFull) json.JsArray(Seq())
      else o.singleton.fold[json.JsValue](
        json.JsArray(Seq(json.Json.toJson(o.lowerBound), json.Json.toJson(o.upperBound))))(
        json.Json.toJson(_))
    }

  private object Binder {
    final class KeyNames(k : String => String) {
      val key = k("")
      val empty = k("empty")
      val singleton = k("singleton")
      val lower = k("lower")
      val lowerClosed = k("lower.closed")
      val upper = k("upper")
      val upperClosed = k("upper.closed")
    }

    type Binder[T,E] = ((String, Map[String, String]) => Either[E, Option[T]])
    def bind[T : RangeType,E](keys : KeyNames, params : Map[String, String], bnd : Binder[T,E], bool : Binder[Boolean,E]) : Either[E, Option[Range[T]]] = {
      /* first we take any single string like "[x,y)" apart into components. */
      val pp = params ++ params.get(keys.key).fold[Iterable[(String, String)]](Nil) { rs =>
        val p = Iterable.newBuilder[(String, String)]
        if (rs.equals("empty") || rs.isEmpty)
          p += keys.empty -> "true"
        else if (!(rs.equals("-") || rs.equals(","))) {
          var s = rs
          if (s.startsWith("[") || s.startsWith("(")) {
            p += keys.lowerClosed -> (s.head == '[').toString
            s = s.tail
          }
          if (s.endsWith("]") || s.endsWith(")")) {
            p += keys.upperClosed -> (s.last == ']').toString
            s = s.init
          }
          val i = Maybe(s.indexOf(',')) orElse s.indexOf('-', 1)
          if (i < 0)
            p += keys.singleton -> s
          else {
            if (i > 0)
              p += keys.lower -> s.substring(0, i)
            if (i+1 < s.length)
              p += keys.upper -> s.substring(i+1)
          }
        }
        p.result
      }
      /* next we parse the components, either from the input parameters or parsed above. */
      for {
        em <- bool(keys.empty, pp).right.map(_.getOrElse(false)).right
        sn <- bnd(keys.singleton, pp).right
        lb <- bnd(keys.lower, pp).right
        ub <- bnd(keys.upper, pp).right
        lc <- bool(keys.lowerClosed, pp).right
        uc <- bool(keys.upperClosed, pp).right
      } yield {
        if ((em && sn.isDefined)
            || lc.isDefined != uc.isDefined
            || ((em || sn.isDefined) && (lb.isDefined || ub.isDefined || lc.isDefined))
            || lb.exists(l => ub.exists(implicitly[RangeType[T]].gt(l, _))))
          None
        else Some {
          if (em) empty[T]
          else sn.fold {
            zip[Boolean, Boolean, Range[T]](lc, uc, apply[T](_, lb, ub, _))
            .getOrElse(apply[T](lb, ub))
          } (singleton[T](_))
        }
      }
    }
    type Unbinder[T, B] = ((String, T) => B)
    def unbind[T : RangeType, B](keys : KeyNames, range : Range[T], bnd : Unbinder[T, B], bool : Unbinder[Boolean, B]) : Seq[B] = {
      if (range.isEmpty) Seq(bool(keys.empty, true))
      else range.singleton.fold {
        val norm = range.normalize
        val isnorm = norm.isNormalized
        Seq[Option[B]](
          norm.lowerBound.map(bnd(keys.lower, _)),
          norm.upperBound.map(bnd(keys.upper, _)),
          if (isnorm) None else Some(bool(keys.lowerClosed, norm.lowerClosed)),
          if (isnorm) None else Some(bool(keys.upperClosed, norm.upperClosed))
        ).flatten
      } (v => Seq(bnd(keys.singleton, v)))
    }
  }

  private[this] def io[E,A](x : Option[Either[E, A]]) : Either[E, Option[A]] = x match {
    case None => Right(None)
    case Some(Left(e)) => Left(e)
    case Some(Right(v)) => Right(Some(v))
  }
  /** This never results in missing parameters: it defaults to FullRange. */
  implicit def queryStringBindable[T : RangeType](implicit bb : QueryStringBindable[T]) : QueryStringBindable[Range[T]] = new QueryStringBindable[Range[T]] {
    /* if using the default "range" name, promote parameters to the top level (messy!). */
    private[this] def basekeys(base : String) = new Binder.KeyNames(base match {
      case "range" => {
        case "" => base
        case key => key
      }
      case "segment" => {
        case "lower" => "start"
        case "upper" => "end"
        case "singleton" => "offset"
        case "" => "segment"
        case key => "segment." + key
      }
      case base => {
        case "" => base
        case key => base + "." + key
      }
    })
    private[this] def bnd(key : String, pp : Map[String, String]) : Either[String, Option[T]] =
      io(bb.bind(key, pp.mapValues(Seq(_))))
    private[this] def bool(key : String, pp : Map[String, String]) : Either[String, Option[Boolean]] =
      io(implicitly[QueryStringBindable[Boolean]].bind(key, pp.mapValues(Seq(_))))
    def bind(key : String, params : Map[String, Seq[String]]) : Option[Either[String, Range[T]]] =
      Some(Binder.bind(basekeys(key), params.mapValues(_.head), bnd, bool)
        .right.flatMap(_.toRight("invalid combination of range parameters for " + key)))
    def unbind(key : String, range : Range[T]) : String =
      Binder.unbind(basekeys(key), range, bb.unbind, QueryStringBindable.bindableBoolean.unbind).mkString(";")
  }

  implicit def formatter[T : RangeType](implicit bf : Formatter[T]) : Formatter[Range[T]] = new Formatter[Range[T]] {
    override val format = bf.format
    private def basekeys(k : String) = new Binder.KeyNames(s => if (s.nonEmpty) k+"."+s else k)
    private[this] def bnd(key : String, pp : Map[String, String]) : Either[Seq[FormError], Option[T]] =
      if (pp.keys.exists(p => (p == key || p.startsWith(key + ".") || p.startsWith(key + "[")) && pp.get(p).exists(_.nonEmpty)))
        bf.bind(key, pp).right.map(Some(_))
      else
        Right(None)
    private[this] def bool(key : String, pp : Map[String, String]) : Either[Seq[FormError], Option[Boolean]] =
      if (pp.contains(key))
        booleanFormat.bind(key, pp).right.map(Some(_))
      else
        Right(None)
    def bind(key : String, data : Map[String, String]) : Either[Seq[FormError], Range[T]] =
      Binder.bind(basekeys(key), data, bnd, bool)
      .right.flatMap(_.toRight(Seq(FormError(key, "invalid combination of range parameters"))))
    def unbind(key : String, value : Range[T]) : Map[String, String] =
      Binder.unbind(basekeys(key), value, bf.unbind, booleanFormat.unbind)
      .fold(Map.empty)(_ ++ _)
  }
}

object Segment {
  val empty : Segment = Range.empty[Offset]
  val full : Segment = Range.full[Offset]
  def singleton(x : Offset) : Section = Range.singleton[Offset](x)
  def apply(lb : Offset, ub : Offset) : Section = Range.apply[Offset](lb, ub)
}

abstract class PGRangeType[A](name : String)(implicit base : SQL.Type[A]) extends RangeType[A] {
  implicit val sqlType = SQL.Type[Range[A]](name, classOf[Range[A]])({ s =>
    if (s.equals("empty") || s.isEmpty)
      Some(Range.empty[A](this))
    else for {
      lc <- s.head match {
        case '[' => Some(true)
        case '(' => Some(false)
        case _ => None
      }
      c <- Maybe(s.indexOf(',', 1))
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
  implicit object segment extends PGRangeType[Offset]("segment") with NumericRangeType[Offset] with Offset.numeric

  implicit object date extends PGRangeType[Date]("daterange") with DiscreteRangeType[Date] {
    def compare(a : Date, b : Date) = a compareTo b
    def increment(a : Date) = a.plusDays(1)
    def decrement(a : Date) = a.minusDays(1)
  }
}
