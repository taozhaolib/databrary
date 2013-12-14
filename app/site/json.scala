package site

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.json._
import play.api.http.Writeable
import macros._

sealed trait JsField {
  def name : String
  def value : JsValue
}

final class JsonField(val name : String, val value : JsValue) extends Tuple2[String, JsValue](name, value) with JsField

object JsonField {
  def apply[J](name : String, value : J)(implicit w : Writes[J]) : JsonField =
    new JsonField(name, w.writes(value))
  import scala.language.implicitConversions
  implicit def ofTuple[J](x : (Symbol, J))(implicit w : Writes[J]) : JsonField =
    new JsonField(x._1.name, w.writes(x._2))
  implicit def ofField(f : JsField) : JsonField =
    new JsonField(f.name, f.value)
}

object JsonObject {
  def apply(fields : JsonField*) =
    new JsObject(fields)
  def flatten(fields : Option[JsonField]*) =
    new JsObject(fields.flatten)
}

final class JsonRecord(val id : JsValue, fields : Seq[(String, JsValue)]) extends JsField {
  def name = id.toString
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
  def seq(s : Seq[JsonRecord]) : JsValue =
    // JsObject(s.map(JsonField.ofField(_))) /* FIXME */
    JsArray(s.map(_.obj))
}

object JsonOptions {
  type Key = String
  type Opt = Seq[String]
  type Value = Opt => Future[JsValue]
  type Tuple = (Key, Value)
  type Options = Map[String, Opt]
  def run(options : Options, opts : Tuple*)(implicit executionContext : ExecutionContext) : Future[JsObject] =
    Async.map[Tuple, Option[JsonField], Seq[Option[JsonField]]](opts, { case (key, fun) =>
      Async.map[Seq[String], JsonField](options.get(key), fun(_).map(JsonField(key, _)))
    }).map(r => new JsObject(r.flatten))
  def apply(base : JsonRecord, options : Options, opts : Tuple*)(implicit exceutionContext : ExecutionContext) : Future[JsonRecord] =
    run(options, opts : _*).map(base ++ _)
}
