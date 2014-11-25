package site

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.json._
import play.api.http.Writeable
import macros._
import macros.async._

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

trait JsonValue {
  def js : JsValue
  override def toString = js.toString
}

object JsonValue {
  implicit val writes : Writes[JsonValue] =
    Writes[JsonValue](_.js)
  implicit def writable(implicit codec : play.api.mvc.Codec) : Writeable[JsonValue] =
    Writeable.writeableOf_JsValue(codec).map(_.js)
}

class JsonObject(val fields : Seq[(String, JsValue)]) extends JsonValue {
  def obj : JsObject = JsObject(fields)
  def js : JsValue = obj
  def +(field : JsonField) =
    new JsonObject(fields :+ field)
  def ++(list : Traversable[JsonField]) =
    new JsonObject(fields ++ list)
  def ++(obj : JsObject) =
    new JsonObject(fields ++ obj.fields)
  def ++(obj : JsonObject) =
    new JsonObject(fields ++ obj.fields)
  def -(field : String) =
    new JsonObject(fields.filterNot(_._1.equals(field)))
}

object JsonObject {
  def apply(fields : JsonField*) =
    new JsonObject(fields)
  def flatten(fields : Option[JsonField]*) =
    new JsonObject(fields.flatten)
  implicit val writes : OWrites[JsonObject] =
    OWrites[JsonObject](_.obj)
}

final class JsonRecord(val id : JsValue, fields : Seq[(String, JsValue)]) extends JsonObject(("id" -> id) +: fields) with JsonValue with JsField {
  def name = id match {
    case JsString(s) => s
    case j => j.toString
  }
  def value = super.js
  override def js = if (fields.isEmpty) id else super.js
  override def +(field : JsonField) =
    new JsonRecord(id, fields :+ field)
  override def ++(list : Traversable[JsonField]) =
    new JsonRecord(id, fields ++ list)
  override def ++(obj : JsObject) =
    new JsonRecord(id, fields ++ obj.fields)
  override def ++(obj : JsonObject) =
    new JsonRecord(id, fields ++ obj.fields)
  override def -(field : String) =
    new JsonRecord(id, fields.filterNot(_._1.equals(field)))
}

object JsonRecord {
  def apply[I : Writes](id : I, fields : JsonField*) =
    new JsonRecord(Json.toJson(id), fields)
  def flatten[I : Writes](id : I, fields : Option[JsonField]*) =
    new JsonRecord(Json.toJson(id), fields.flatten)
  def seq(s : Seq[JsonRecord]) : JsValue =
    JsObject(s.map(JsonField.ofField(_)))

  implicit object SeqBuild extends CanBuildFrom[Iterable[_], JsonRecord, JsValue] {
    final class SeqBuilder extends scala.collection.mutable.Builder[JsonRecord, JsValue] {
      private[this] val builder = Seq.newBuilder[JsonField]
      def +=(e : JsonRecord) = {
        builder += JsonField.ofField(e)
        this
      }
      def clear() = builder.clear
      def result() = JsObject(builder.result)
    }
    def apply(coll : Iterable[_]) = new SeqBuilder
    def apply() = new SeqBuilder
  }

  def map[A](f : A => JsonRecord)(l : Iterable[A]) : JsValue =
    l.map(f)
}

object JsonArray {
  /** Stupidly duplicates the existing Writes[Seq[J : Writes]]. */
  def apply[J](l : Seq[J])(implicit w : Writes[J]) : JsValue =
    JsArray(l.map(w.writes))
  def map[A,J](f : A => J)(l : Iterable[A])(implicit w : Writes[J]) : JsValue =
    JsArray(l.map(f.andThen(w.writes)).toSeq)
}

object JsonOptions {
  type Key = String
  type Opt = Seq[String]
  type Value = Opt => Future[JsValue]
  type Tuple = (Key, Value)
  type Options = Map[String, Opt]
  def run(options : Options, opts : Tuple*)(implicit executionContext : ExecutionContext) : Future[JsObject] =
    opts.mapAsync({ case (key, fun) =>
      options.get(key).mapAsync(fun(_).map(JsonField(key, _)))
    }).map(r => new JsObject(r.flatten))
  def apply(base : JsonRecord, options : Options, opts : Tuple*)(implicit exceutionContext : ExecutionContext) : Future[JsonRecord] =
    run(options, opts : _*).map(base ++ _)
  def apply(base : JsObject, options : Options, opts : Tuple*)(implicit exceutionContext : ExecutionContext) : Future[JsObject] =
    run(options, opts : _*).map(base ++ _)
}
