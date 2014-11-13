package ingest

import scala.concurrent.Future
import play.api.libs.json
import macros.async._
import dbrary._
import site._

case class JsonException(errors: Seq[(json.JsPath, Seq[play.api.data.validation.ValidationError])])
  extends IngestException(errors.mkString("\n"))

private final case class JsContext(path : json.JsPath, data : json.JsValue) {
  def \(child : String) = JsContext(path \ child, data \ child)
  def apply(idx : Int) = JsContext(path(idx), data(idx))
  def as[A](implicit read : json.Reads[A]) : A = 
    read.reads(data).repath(path) match {
      case json.JsSuccess(v, _) => v
      case json.JsError(e) => throw JsonException(e)
    }
  def asOpt[A](implicit read : json.Reads[A]) : Option[A] = 
    if (data.isInstanceOf[json.JsUndefined])
      None
    else
      Some(as[A](read))
}

private object Json {
  private[this] lazy val schema =
    com.github.fge.jsonschema.main.JsonSchemaFactory.byDefault.getJsonSchema(
      com.github.fge.jackson.JsonLoader.fromResource("/volume.json"))

  /* FIXME in play 2.4 this has been implemented properly: */
  private[this] def jsonToNode(j : json.JsValue) =
    new com.fasterxml.jackson.databind.ObjectMapper().readTree(json.Json.stringify(j))

  private def validate(j : json.JsValue) {
    val v = schema.validate(jsonToNode(j))
    if (!v.isSuccess)
      throw new IngestException(v.toString)
  }
}

final class Json(v : models.Volume, data : json.JsValue, overwrite : Boolean = false)(implicit site : Site) extends Ingest {
  import Parse._

  Json.validate(data)

  private[this] def popErr(msg : String)(implicit jc : JsContext) =
    Future.failed(PopulateException(msg + " in " + jc.path.toString))

  private[this] def write[A](current : Option[A], jc : JsContext)(change : A => Future[Boolean])(implicit read : json.Reads[A]) : Future[Unit] =
    jc.asOpt[A](read).fold(void) { v =>
      if (current.exists(_.equals(v)))
        void
      else if (current.isEmpty || overwrite)
        change(v).flatMap { r =>
          if (r) void else popErr("update failed")(jc)
        }
      else
        popErr("conflicting value: " + v + " <> " + current)(jc)
    }

  private[this] def container(implicit jc : JsContext) : Future[models.Container] = {
    val key = (jc \ "key").as[String]
    for {
      c <- models.Container.rowVolume(v)
        .SELECT("JOIN ingest.container USING (id, volume) WHERE key = ?")
        .apply(key).singleOpt
      c <- c.fold {
        for {
          c <- models.Container.create(v,
            top = (jc \ "top").asOpt[Boolean].getOrElse(false),
            name = (jc \ "name").asOpt[String],
            date = (jc \ "date").asOpt[Date])
          _ <- SQL("INSERT INTO ingest.container (id, volume, key) VALUES (?, ?, ?)")
            .apply(c.id, v.id, key).execute
        } yield (c)
      } { c => for {
          _ <- write(Some(c.top), jc \ "top")(_ => popErr("can't change container top"))
          _ <- write(c.name, jc \ "name")(x => c.change(name = Some(Some(x))))
          _ <- write(c.date, jc \ "date")(x => c.change(date = Some(Some(x))))
        } yield (c)
      }

      consents = jc \ "consent"
    } yield (c)
  }

  def run() : Future[Unit] = {
    implicit val jc = JsContext(json.__, data)
    for {
      /* just to make sure it's the right volume: */
      _ <- write(Some(v.name), jc \ "name")(x => /* v.change(name = Some(x)) */
          popErr("refusing to overwrite mismatching volume name"))
      /* We don't actually ingest volume-level metadata through this interface:
      _ <- write(v.body, jc \ "body")(x => v.change(body = Some(Some(x))))
      _ <- write(v.alias, jc \ "alias")(x => v.change(alias = Some(Some(x))))
      cite <- v.citation
      ...
      */
     
      _ <- (data \ "containers").asOpt[json.JsArray].fold[Seq[json.JsValue]](Nil)(_.value)
        .zipWithIndex.foreachAsync { case (j, i) =>
          container(JsContext(json.__(i), j))
        }
    } yield ()
  }
}
