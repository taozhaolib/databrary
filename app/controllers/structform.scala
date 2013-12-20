package controllers

import scala.collection.mutable
import play.api.data._
import play.api.data.validation._

abstract class StructData {
  self =>

  private def getValFields =
    getClass.getDeclaredFields.
      filter { f =>
        classOf[StructMember[_]].isAssignableFrom(f.getType)
        // && (getClass.getDeclaredMethod(f.getName).invoke(self).asInstanceOf[StructMember[_]].struct eq self)
      }.map(_.getName)
  private val valFields : Iterator[String] = getValFields.iterator

  private val fieldMap : mutable.Map[String, StructMember[_]] = mutable.Map.empty[String, StructMember[_]]
  protected sealed abstract class StructMember[T] private[StructData] (val name : String, _map : Mapping[T]) {
    fieldMap.put(name, this).ensuring(_.isEmpty, "Duplicate field: " + name)
    private[StructData] final val struct = self
    def value : T
    protected val map : Mapping[T] = _map.withPrefix(name)
    final def mapping(key : String) = map.withPrefix(key)
    def bind(key : String, data : Map[String,String]) : Option[Seq[FormError]]
    final def unbind(key : String) = mapping(key).unbind(value)
    final def field(form : Form[_]) = form(name)
  }
  protected sealed class StructField[T] private[StructData] (name : String, _map : Mapping[T]) extends StructMember[T](name, _map) {
    var value : T = _
    def bind(key : String, data : Map[String,String]) : Option[Seq[FormError]] =
      mapping(key).bind(data).fold(Some(_), v => { value = v ; None })
    final def withName(name : String) = new StructField[T](name, _map)
    final def defaulting(init : T) = new StructFieldDefault[T](name, _map, init)
  }
  protected sealed class StructFieldDefault[T] private[StructData] (name : String, _map : Mapping[T], init : T) extends StructField[T](name, _map) {
    value = init
    override def bind(key : String, data : Map[String,String]) : Option[Seq[FormError]] = {
      val k = mapping(key).key
      if (data.contains(k) || data.keys.exists(p => p.startsWith(k + ".") || p.startsWith(k + "[")))
        super.bind(key, data)
      else
        None
    }
  }
  /*
  protected final class StructNested[T >: StructData <: StructData] private[StructData] (name : String, val struct : T) extends StructMember[T](name, struct.mapping) {
    val value = struct
    def bind(key : String, data : Map[String,String]) : Option[Seq[FormError]] =
      mapping(key).bind(data).left.toOption
  }
  */

  protected def Field[T](mapping : Mapping[T], name : String = valFields.next) =
    new StructField[T](name, mapping)

  protected case class StructMapping private[StructData] (key : String = "", constraints : Seq[Constraint[self.type]] = Nil) extends Mapping[self.type] {
    private val fields = fieldMap.values.toSeq
    def bind(data : Map[String,String]) : Either[Seq[FormError], self.type] = {
      val l = fields.flatMap(_.bind(key, data))
      if (l.isEmpty)
        Right(self)
      else
        Left(l.flatten)
    }
    def unbind(value : self.type) : (Map[String, String], Seq[FormError]) = {
      val (m, e) = fields.map(_.unbind(key)).unzip
      (m.fold(Map.empty[String, String])(_ ++ _), e.flatten[FormError])
    }
    def withPrefix(prefix : String) : StructMapping =
      addPrefix(prefix).fold(this)(k => copy(key = k))
    def verifying(c : Constraint[self.type]*) : StructMapping =
      copy(constraints = constraints ++ c)
    val mappings : Seq[Mapping[_]] =
      this +: fields.flatMap(_.mapping(key).mappings)
  }
  def mapping = new StructMapping

  class StructForm private[StructData] (mapping : StructMapping, data : Map[String, String], errors : Seq[FormError], value : Option[self.type]) extends Form[self.type](mapping, data, errors, value) {
    def field(f : self.type => StructMember[_]) : Field = f(self).field(this)
  }
  def form = new StructForm(mapping, Map.empty, Nil, None)
}
