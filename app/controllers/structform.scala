package controllers

import scala.collection.mutable
import play.api.data._
import play.api.data.validation._

abstract class StructForm {
  self =>

  private def getValFields =
    getClass.getDeclaredFields.
      filter { f =>
        classOf[StructMember[_]].isAssignableFrom(f.getType)
        // && (getClass.getDeclaredMethod(f.getName).invoke(self).asInstanceOf[StructMember[_]].struct eq self)
      }.map(_.getName)
  private val valFields : Iterator[String] = getValFields.iterator

  private val fieldMap : mutable.Map[String, StructMember[_]] = mutable.Map.empty[String, StructMember[_]]
  protected sealed abstract class StructMember[T] private[StructForm] (val name : String, _map : Mapping[T]) {
    fieldMap.put(name, this).ensuring(_.isEmpty, "Duplicate field: " + name)
    private[StructForm] final val struct = self
    def value : T
    protected val map : Mapping[T] = _map.withPrefix(name)
    final def mapping(key : String) = map.withPrefix(key)
    def bind(key : String, data : Map[String,String]) : Option[Seq[FormError]]
    final def unbind(key : String) = mapping(key).unbind(value)
    final def field = self.apply.form(name) // XXX nested structs
  }
  protected sealed class StructField[T] private[StructForm] (name : String, _map : Mapping[T]) extends StructMember[T](name, _map) {
    var value : T = _
    def bind(key : String, data : Map[String,String]) : Option[Seq[FormError]] =
      mapping(key).bind(data).fold(Some(_), v => { value = v ; None })
    def withName(name : String) = new StructField[T](name, _map)
    final def defaulting(init : T) = new StructFieldDefault[T](name, _map, init)
  }
  protected sealed class StructFieldDefault[T] private[StructForm] (name : String, _map : Mapping[T], init : T) extends StructField[T](name, _map) {
    value = init
    override def bind(key : String, data : Map[String,String]) : Option[Seq[FormError]] = {
      val k = mapping(key).key
      if (data.contains(k) || data.keys.exists(p => p.startsWith(k + ".") || p.startsWith(k + "[")))
        super.bind(key, data)
      else
        None
    }
    override def withName(name : String) = new StructFieldDefault[T](name, _map, init)
  }
  protected final class StructNested[T <: StructForm] private[StructForm] (name : String, _map : T#StructMapping) extends StructMember[T](name, _map.asInstanceOf[Mapping[T]]) {
    val value = _map.struct
    def bind(key : String, data : Map[String,String]) : Option[Seq[FormError]] =
      mapping(key).bind(data).left.toOption
  }

  protected def Field[T](mapping : Mapping[T]) : StructField[T] =
    new StructField[T](valFields.next, mapping)
  protected def Field[T](name : String, mapping : Mapping[T]) : StructField[T] = {
    valFields.next
    new StructField[T](name, mapping)
  }
  protected def Nested[T <: StructForm](mapping : T#StructMapping) : StructNested[T] =
    new StructNested[T](valFields.next, mapping)
  protected def Nested[T <: StructForm](name : String, mapping : T#StructMapping) : StructNested[T] = {
    valFields.next
    new StructNested[T](name, mapping)
  }

  protected case class StructMapping private[StructForm] (key : String = "", constraints : Seq[Constraint[self.type]] = Nil) extends Mapping[self.type] {
    private val fields = fieldMap.values.toSeq
    def struct : self.type = self
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

    protected[StructMapping] class StructMappingForm private[StructMapping] (mapping : StructMapping, data : Map[String, String], errors : Seq[FormError], value : Option[self.type]) extends Form[self.type](mapping, data, errors, value)
    def form = new StructMappingForm(this, Map.empty, Nil, None)
  }

  def apply() = new StructMapping
}
