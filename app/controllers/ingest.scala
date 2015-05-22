package controllers

import scala.concurrent.{ExecutionContext,Future}
import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import macros._
import site._
import models._
import ingest._

object IngestController extends SiteController with HtmlController {
  private implicit val context : ExecutionContext = site.context.background

  private def Action(i : models.Volume.Id) =
    SiteAction.rootMember() andThen VolumeController.action(i, Permission.EDIT)

  final class JsonForm(implicit request : VolumeController.Request[_])
    extends HtmlForm[JsonForm](
      routes.IngestController.json(request.obj.id),
      views.html.ingest.json(_)) {
    val json = File()
      .verifying(validation.Constraint[FilePart] { f : FilePart =>
        if (!(f.contentType.exists(c => c.equals("text/json") || c.equals("application/json")) || f.filename.endsWith(".json")))
          validation.Invalid(validation.ValidationError("file.format.unknown", f.contentType.getOrElse(f.filename)))
        else validation.Valid
      })
    val run = Field(Forms.boolean)
    val overwrite = Field(Forms.boolean).fill(false)
  }

  def jsonView(v : models.Volume.Id) = Action(v).async { implicit request =>
    new JsonForm().Ok
  }

  def json(v : models.Volume.Id) = Action(v).async { implicit request =>
    val volume = request.obj
    val form = new JsonForm()._bind
    val json = try {
      play.api.libs.json.Json.parse(org.apache.commons.io.FileUtils.readFileToByteArray(form.json.get.ref.file))
    } catch {
      case e : com.fasterxml.jackson.core.JsonParseException => form.json.withError(e.getMessage)._throw
    }
    async.catching(classOf[IngestException]) {
      val in = new ingest.Json(request.obj, json, form.overwrite.get)
      (if (form.run.get)
        in.run()
      else
        async(Nil))
      .map(r => Ok(views.html.ingest.json(form, "done", r)))
    }.recover { case e : IngestException =>
      BadRequest(views.html.ingest.json(form, e.getMessage, e.target))
    }
  }

}
