package controllers

import scala.collection.mutable
import play.api.data._
import play.api.data.validation._

abstract class StructForm(val action : play.api.mvc.Call) {
  self =>

  /** A field in this form, which should only be used to declare vals. */
  protected final case class Field[T](mapping : Mapping[T]) {
    /** The value of this field, which will be filled in by binding the form. */
    var value : T = _
    def init(v : T) : Field[T] = {
      value = v
      this
    }
  }

  private[this] sealed class ValField[T](val name : String, val field : Field[T]) {
    private[StructForm] def mapping : Mapping[T] = field.mapping.withPrefix(name)
    private[StructForm] final def bind(data : Map[String,String]) : Option[Seq[FormError]] =
      mapping.bind(data).fold(Some(_), v => { field.value = v ; None })
    private[StructForm] final def unbind = mapping.unbind(field.value)
  }
  private[this] def valField(v : java.lang.reflect.Field) : ValField[_] = {
    val name = v.getName
    new ValField(name, getClass.getDeclaredMethod(name).invoke(self).asInstanceOf[Field[_]])
  }
  private[this] def getValFields : Array[ValField[_]] =
    getClass.getDeclaredFields.
      filter { f =>
        classOf[Field[_]].isAssignableFrom(f.getType)
      }.map(valField)
  private lazy val valFields = getValFields

  protected case class mapping private[StructForm] (key : String = "", constraints : Seq[Constraint[self.type]] = Nil) extends Mapping[self.type] {
    private[this] final class MappingField[T](name : String, field : Field[T]) extends ValField[T](name, field) {
      final val mapping = super.mapping.withPrefix(key)
    }
    private[this] def mappingField[T](f : ValField[T]) =
      new MappingField[T](f.name, f.field)
    private val fields = valFields.map[MappingField[_], Seq[MappingField[_]]](mappingField(_))

    def bind(data : Map[String,String]) : Either[Seq[FormError], self.type] = {
      val l = fields.flatMap(_.bind(data))
      if (l.isEmpty)
        Right(self)
      else
        Left(l.flatten)
    }
    def unbind(value : self.type) : (Map[String, String], Seq[FormError]) = {
      val (m, e) = fields.map(_.unbind).unzip
      (m.fold(Map.empty[String, String])(_ ++ _), e.flatten[FormError])
    }
    def withPrefix(prefix : String) : mapping =
      addPrefix(prefix).fold(this)(k => copy(key = k))
    def verifying(c : Constraint[self.type]*) : mapping =
      copy(constraints = constraints ++ c)
    val mappings : Seq[Mapping[_]] =
      this +: fields.flatMap(_.mapping.mappings)

    protected class form private[mapping] (mapping : mapping, data : Map[String, String], errors : Seq[FormError], value : Option[self.type]) extends Form[self.type](mapping, data, errors, value)
    def form = new form(this, Map.empty, Nil, None)
  }

  def apply() = (new mapping).form
}
