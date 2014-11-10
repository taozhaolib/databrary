package ingest

import play.api.libs.json

object Json extends Ingest {
  import Parse._

  private lazy val schema =
    com.github.fge.jsonschema.main.JsonSchemaFactory.byDefault.getJsonSchema(
      com.github.fge.jackson.JsonLoader.fromResource("/volume.json"))

  /* FIXME in play 2.4 this has been implemented properly: */
  private def jsonToNode(j : json.JsValue) =
    new com.fasterxml.jackson.databind.ObjectMapper().readTree(json.Json.stringify(j))

  def load(j : json.JsValue) : Unit = {
    val v = schema.validate(jsonToNode(j))
    if (!v.isSuccess)
      throw new IngestException(v.toString)
  }
}
