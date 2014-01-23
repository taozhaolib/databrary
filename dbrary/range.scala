package dbrary

import play.api.libs.json
import play.api.mvc.QueryStringBindable
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
	  if (c == 0) y.lowerClosed compare x.lowerClosed
	  else c
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
	  if (c == 0) x.lowerClosed compare y.lowerClosed
	  else c
	}).getOrElse(xb.isEmpty compare yb.isEmpty)
      }
    }
  }

}
trait DiscreteRangeType[A] extends RangeType[A] {
  def increment(a : A) : A // = a + 1
  def decrement(a : A) : A // = a - 1
  override final def isDiscrete = true
}

object RangeType {
  implicit object int extends DiscreteRangeType[Int] with Ordering.IntOrdering {
    def increment(a : Int) = a + 1
    def decrement(a : Int) = a - 1
  }
  implicit object long extends DiscreteRangeType[Long] with Ordering.LongOrdering {
    def increment(a : Long) = a + 1
    def decrement(a : Long) = a - 1
  }
  implicit val segment : RangeType[Offset] = PGRangeType.segment
  implicit val date : RangeType[Date] = PGRangeType.date
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
  /** Contains relations, as in postgres. */
  final def @>(x : A) = isFull || !isEmpty &&
    lowerBound.fold(true)(l => if (lowerClosed) t.lteq(l, x) else t.lt(l, x)) &&
    upperBound.fold(true)(u => if (upperClosed) t.lteq(x, u) else t.lt(x, u))
  final def @>(r : Range[A]) = isFull || r.isEmpty || !isEmpty &&
    t.LowerOrdering.lteq(this, r) && t.UpperOrdering.gteq(this, r)
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
  final def ===(a : Range[A]) = a match {
    case r : Range[A] =>
      val oo = Ordering.Option(t)
      oo.equiv(lowerBound, r.lowerBound) &&
      oo.equiv(upperBound, r.upperBound) &&
      lowerClosed == r.lowerClosed &&
      upperClosed == r.upperClosed &&
      isEmpty == r.isEmpty
    case _ => false
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

  implicit def lowerOrdering[A](implicit t : RangeType[A]) : Ordering[Range[A]] = t.LowerOrdering
  implicit def upperOrdering[A](implicit t : RangeType[A]) : Ordering[Range[A]] = t.UpperOrdering
  implicit val segmentSqlType : SQLType[Range[Offset]] = PGRangeType.segment.sqlType
  implicit val dateSqlType : SQLType[Range[Date]] = PGRangeType.date.sqlType
  implicit def jsonWrites[T : json.Writes] : json.Writes[Range[T]] =
    json.Writes[Range[T]] { o =>
      if (o.isEmpty) json.JsNull
      else o.singleton.fold[json.JsValue](
        json.JsArray(Seq(json.Json.toJson(o.lowerBound), json.Json.toJson(o.upperBound))))(
        json.Json.toJson(_))
    }
  /** This never results in missing parameters: it defaults to FullRange. */
  implicit def queryStringBindable[T : RangeType](implicit bb : QueryStringBindable[T]) : QueryStringBindable[Range[T]] = new QueryStringBindable[Range[T]] {
    /* if using the default "range" name, promote parameters to the top level (messy!). */
    private[this] def basekey(base : String) : String => String = base match {
      case "range" => identity
      case "segment" => {
        case "lower" => "start"
        case "upper" => "end"
        case "singleton" => "offset"
        case key => "segment." + key
      }
      case base => base + "." + _
    }
    private[this] def io[A](x : Option[Either[String, A]]) : Either.RightProjection[String, Option[A]] = Either.RightProjection(x match {
      case None => Right(None)
      case Some(Left(e)) => Left(e)
      case Some(Right(v)) => Right(Some(v))
    })
    def bind(key : String, params : Map[String, Seq[String]]) : Option[Either[String, Range[T]]] = {
      val keyname = basekey(key)
      /* first we take any single string like "[x,y)" apart into components. */
      val pp = params ++ params.get(key).flatMap(_.headOption).fold[Iterable[(String, Seq[String])]](Nil) { rs =>
        val p = Iterable.newBuilder[(String, Seq[String])]
        def add(key : String, v : String) =
          p += (keyname(key) -> Seq(v))
        if (rs.equals("empty") || rs.isEmpty)
          add("empty", "true")
        else {
          var s = rs
          if (s.startsWith("[") || s.startsWith("(")) {
            add("lower.closed", (s.head == '[').toString)
            s = s.tail
          }
          if (s.endsWith("]") || s.endsWith(")")) {
            add("upper.closed", (s.last == ']').toString)
            s = s.init
          }
          val i = Maybe(s.indexOf(',')) orElse s.indexOf('-')
          if (i < 0)
            add("singleton", s)
          else {
            if (i > 0)
              add("lower", s.substring(0, i))
            if (i+1 < s.length)
              add("upper", s.substring(i+1))
          }
        }
        p.result
      }
      /* next we parse the components, either from the input parameters or parsed above. */
      def bnd(key : String) : Either.RightProjection[String, Option[T]] =
        io(bb.bind(keyname(key), pp))
      def bool(key : String) : Either.RightProjection[String, Option[Boolean]] =
        io(QueryStringBindable.bindableBoolean.bind(keyname(key), pp))
      Some(for {
        em <- bool("empty").map(_.getOrElse(false)).right
        sn <- bnd("singleton")
        lb <- bnd("lower")
        ub <- bnd("upper")
        lc <- bool("lower.closed")
        uc <- bool("upper.closed")
        _ <- Either.RightProjection(
          if ((em && sn.isDefined)
              || lc.isDefined != uc.isDefined
              || ((em || sn.isDefined) && (lb.isDefined || ub.isDefined || lc.isDefined)))
            Left("invalid combination of range parameters for " + key)
          else Right(()))
      } yield (
        if (em) empty[T]
        else sn.fold {
          zip[Boolean, Boolean, Range[T]](lc, uc, apply[T](_, lb, ub, _))
	  .getOrElse(apply[T](lb, ub))
        } (singleton[T](_))
      ))
    }
    def unbind(key : String, range : Range[T]) : String = {
      val keyname = basekey(key)
      def bnd(key : String, x : T) =
        bb.unbind(keyname(key), x)
      def bool(key : String, x : Boolean) =
        QueryStringBindable.bindableBoolean.unbind(keyname(key), x)
      if (range.isEmpty) bool("empty", true)
      else range.singleton.fold {
        val norm = range.normalize
        val isnorm = norm.isNormalized
        Seq[Option[String]](
          norm.lowerBound.map(bnd("lower", _)),
          norm.upperBound.map(bnd("upper", _)),
          if (isnorm) None else Some(bool("lower.closed", norm.lowerClosed)),
          if (isnorm) None else Some(bool("upper.closed", norm.upperClosed))
        ).flatten.mkString(";")
      } (bb.unbind(keyname("singleton"), _))
    }
  }
}

object Segment {
  def singleton(x : Offset) : Section = Range.singleton[Offset](x)
  def apply(lb : Offset, ub : Offset) : Section = Range.apply[Offset](lb, ub)
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

  implicit object date extends PGRangeType[Date]("daterange") with DiscreteRangeType[Date] {
    def compare(a : Date, b : Date) = a compareTo b
    def increment(a : Date) = a.plusDays(1)
    def decrement(a : Date) = a.minusDays(1)
  }
}
