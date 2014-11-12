package ingest

import scala.concurrent.Future
import play.api.libs.json
import macros._

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

class Json(v : models.Volume, j : json.JsValue, overwrite : Boolean = false) extends Ingest {
  import Parse._

  Json.validate(j)

  private def write[A](current : A, value : A)(change : A => Future[Unit]) : Future[Unit] =
    if (current.equals(value))
      async.void
    else if (overwrite)
      change(value)
    else
      Future.failed(PopulateException("conflicting value: " + value.toString + " <> " + current.toString))

  def run() : Future[Unit] =
    async.void
}
