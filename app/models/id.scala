package models

import play.api.data.format.{Formats,Formatter}
import play.api.mvc.{PathBindable,QueryStringBindable}
import play.api.libs.json
import dbrary._

/** Wrap identifiers and tag them with a particular type. This is primarily useful to tag primary keys with a specific type corresponding to the source table.
  * @tparam I the type of the identifier
  * @tparam T the tag
  */
private[models] abstract class GenericId[I,+T](val unId : I) {
  def ===[X >: T](i : GenericId[I,X]) : Boolean
  /** Equality based on id value.
    * This doesn't properly check types due to erasure, so === should be preferred. */
  override def equals(i : Any) = i match {
    case i : GenericId[I,T] => i.unId equals unId
    case _ => false
  }
  // this is necessary for match:
  def ==(i : GenericId[I,_]) = i.unId == unId
  @deprecated("by ===", "") def !=(i : GenericId[I,_]) = unId != i.unId
  override def hashCode = unId.hashCode
  override def toString = unId.toString
}

/** GenericId specific to integers.  The most common (only?) type of identifier we have. */
final class IntId[+T] private (unId : Int) extends GenericId[Int,T](unId) {
  def ===[X >: T](i : GenericId[Int,X]) = unId == i.unId
  override def hashCode = unId
  /** Forcibly coerce to a different type. */
  private[models] def coerce[A] = new IntId[A](unId)
  def formatted(s : String) = unId.formatted(s)
}
private[models] object IntId {
  private[models] def apply[T](i : Int) = new IntId[T](i)
  // The normal family of conversions for database and web i/o:
  implicit def pathBindable[T] : PathBindable[IntId[T]] = PathBindable.bindableInt.transform(apply[T] _, _.unId)
  implicit def queryStringBindable[T] : QueryStringBindable[IntId[T]] = QueryStringBindable.bindableInt.transform(apply[T] _, _.unId)
  implicit def sqlType[T] : SQLType[IntId[T]] = SQLType.transform[Int,IntId[T]]("integer", classOf[IntId[T]])(i => Some(apply[T](i)), _.unId)
  implicit def formatter[T] : Formatter[IntId[T]] = new Formatter[IntId[T]] {
    def bind(key : String, data : Map[String, String]) =
      Formats.intFormat.bind(key, data).right.map(apply _)
    def unbind(key : String, value : IntId[T]) =
      Formats.intFormat.unbind(key, value.unId)
  }
  implicit def jsonWrites[T] : json.Writes[IntId[T]] = new json.Writes[IntId[T]] {
    def writes(i : IntId[T]) = json.JsNumber(i.unId)
  }
}
/** Any class (usually a singleton object) which provides an Id type. */
private[models] trait HasId[+T] {
  type Id = IntId[T]
  /** Create an [[Id]] value. */
  def asId(i : Int) : Id = IntId[T](i)
}

/** GenericId specific to longs. */
final class LongId[+T] private (unId : Long) extends GenericId[Long,T](unId) {
  def ===[X >: T](i : GenericId[Long,X]) = unId == i.unId
  override def hashCode = unId.hashCode
  /** Forcibly coerce to a different type. */
  private[models] def coerce[A] = new LongId[A](unId)
  def formatted(s : String) = unId.formatted(s)
}
private[models] object LongId {
  private[models] def apply[T](i : Long) = new LongId[T](i)
  // The normal family of conversions for database and web i/o:
  implicit def pathBindable[T] : PathBindable[LongId[T]] = PathBindable.bindableLong.transform(apply[T] _, _.unId)
  implicit def queryStringBindable[T] : QueryStringBindable[LongId[T]] = QueryStringBindable.bindableLong.transform(apply[T] _, _.unId)
  implicit def sqlType[T] : SQLType[LongId[T]] = SQLType.transform[Long,LongId[T]]("bigint", classOf[LongId[T]])(i => Some(apply[T](i)), _.unId)
  implicit def formatter[T] : Formatter[LongId[T]] = new Formatter[LongId[T]] {
    def bind(key : String, data : Map[String, String]) =
      Formats.longFormat.bind(key, data).right.map(apply _)
    def unbind(key : String, value : LongId[T]) =
      Formats.longFormat.unbind(key, value.unId)
  }
  implicit def jsonWrites[T] : json.Writes[LongId[T]] = new json.Writes[LongId[T]] {
    def writes(i : LongId[T]) = json.JsNumber(i.unId)
  }
}
