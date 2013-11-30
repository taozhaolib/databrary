package models

import play.api.data.format.{Formats,Formatter}
import play.api.mvc.{PathBindable,QueryStringBindable}
import dbrary._

/** Wrap identifiers and tag them with a particular type. This is primarily useful to tag primary keys with a specific type corresponding to the source table.
  * @tparam I the type of the identifier
  * @tparam T the tag
  */
private[models] class GenericId[I,+T](val unId : I) {
  // I don't understand why this is necessary (and it's also not quite right with inheritance):
  def equals(i : GenericId[I,_]) = i.unId equals unId
  def ==(i : GenericId[I,_]) = i.unId == unId
  def !=(i : GenericId[I,_]) = !(this == i)
  override def hashCode = unId.hashCode
  override def toString = unId.toString
}
/** [[GenericId]] specific to integers.  The most common (only?) type of identifier we have. */
private[models] final class IntId[+T] private (unId : Int) extends GenericId[Int,T](unId) {
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
}
/** Any class (usually a singleton object) which provides an Id type. */
private[models] trait HasId[+T] {
  type Id = IntId[T]
  /** Create an [[Id]] value. */
  def asId(i : Int) : Id = IntId[T](i)
}

