package site

import play.api.libs.json._

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
  implicit val hashWrites : OWrites[Seq[JsField]] = new OWrites[Seq[JsField]] {
    def writes(s : Seq[JsField]) = JsObject(s.map(JsonField.ofField(_)))
  }
}

sealed class JsonObject(val fields : Seq[JsonField]) {
  def obj = JsObject(fields)
}

object JsonObject {
  def apply(fields : JsonField*) =
    new JsonObject(fields)
  def flatten(fields : Option[JsonField]*) =
    new JsonObject(fields.flatten)
  implicit val writes : OWrites[JsonObject] = new OWrites[JsonObject] {
    def writes(d : JsonObject) = d.obj
  }
}

final class JsonObjectId(val id : JsValue, fields : Seq[JsonField]) extends JsonObject(fields) with JsField {
  def field = id.toString
  def value = JsObject(fields)
  // override def obj = JsObject(("id" -> id) +: fields)
}

object JsonObjectId {
  def apply[I : Writes](id : I, fields : JsonField*) =
    new JsonObjectId(Json.toJson(id), fields)
  def flatten[I : Writes](id : I, fields : Option[JsonField]*) =
    new JsonObjectId(Json.toJson(id), fields.flatten)
}

trait JsonableObject {
  def json(implicit site : Site) : JsonObject
}
trait JsonableObjectId extends JsonableObject {
  def json(implicit site : Site) : JsonObjectId
}
