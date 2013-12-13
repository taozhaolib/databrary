package site

import play.api.libs.json._
import play.api.http.Writeable

sealed trait JsField {
  def field : String
  def value : JsValue
}

final class JsonField(val field : String, val value : JsValue) extends Tuple2[String, JsValue](field, value) with JsField

object JsonField {
  import scala.language.implicitConversions
  implicit def ofTuple[J](x : (Symbol, J))(implicit w : Writes[J]) : JsonField =
    new JsonField(x._1.name, w.writes(x._2))
  implicit def ofField(f : JsField) : JsonField =
    new JsonField(f.field, f.value)
}

object JsField {
  implicit val hashWrites : OWrites[Seq[JsField]] =
    OWrites[Seq[JsField]](s => JsObject(s.map(JsonField.ofField(_))))
}

object JsonObject {
  def apply(fields : JsonField*) =
    new JsObject(fields)
  def flatten(fields : Option[JsonField]*) =
    new JsObject(fields.flatten)
}

final class JsonRecord(val id : JsValue, fields : Seq[(String, JsValue)]) extends JsField {
  def field = id.toString
  def value = JsObject(fields)
  def obj = JsObject(("id" -> id) +: fields)
  def +(field : JsonField) =
    new JsonRecord(id, fields :+ field)
  def ++(list : Traversable[JsonField]) =
    new JsonRecord(id, fields ++ list)
  def ++(obj : JsObject) =
    new JsonRecord(id, fields ++ obj.fields)
  def -(field : String) =
    new JsonRecord(id, fields.filterNot(_._1.equals(field)))
}

object JsonRecord {
  def apply[I : Writes](id : I, fields : JsonField*) =
    new JsonRecord(Json.toJson(id), fields)
  def flatten[I : Writes](id : I, fields : Option[JsonField]*) =
    new JsonRecord(Json.toJson(id), fields.flatten)
  implicit val writes : OWrites[JsonRecord] =
    OWrites[JsonRecord](_.obj)
  implicit def writable(implicit codec : play.api.mvc.Codec) : Writeable[JsonRecord] =
    Writeable.writeableOf_JsValue(codec).map(_.obj)
}

trait JsonableRecord {
  def json(implicit site : Site) : JsonRecord
}
