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
    SiteAction.rootAccess(Permission.ADMIN) andThen VolumeController.action(i, Permission.EDIT)

  trait CSVForm extends StructForm {
    protected def CSV[T <: FileMember[_]](f : T = File()) : T = f
      .verifying(validation.Constraint[FilePart] { f : FilePart =>
        val fmt = AssetFormat.getFilePart(f).map(_.mimetype).orElse(f.contentType)
        if (!fmt.exists(_.equals("text/csv")))
          validation.Invalid(validation.ValidationError("file.format.unknown", fmt.getOrElse("unknown")))
        else validation.Valid
      })
  }

  final class CuratedForm(implicit request : VolumeController.Request[_])
    extends HtmlForm[CuratedForm](
      routes.IngestController.curated(request.obj.id),
      views.html.ingest.curated(_))
    with CSVForm {
    val file = CSV()
    val run = Field(Forms.boolean)
  }

  def curatedView(v : models.Volume.Id) = Action(v).async { implicit request =>
    new CuratedForm().Ok
  }

  def curated(v : models.Volume.Id) = Action(v).async { implicit request =>
    val volume = request.obj
    val form = new CuratedForm()._bind
    if (!form.run.get)
      Future(ingest.Curated.preview(form.file.get.ref.file)).map { r =>
        Ok(views.html.ingest.curated(form, r))
      }.recover {
        case e : IngestException =>
          BadRequest(views.html.ingest.curated(form, e.getMessage))
      }
    else 
      ingest.Curated.populate(form.file.get.ref.file, volume).map { r =>
        Ok(views.html.ingest.result(volume, (r._1 : Iterable[SiteObject]) ++ r._2))
      }.recover {
        case e : PopulateException =>
          Logger.error("curated ingest error", e)
          BadRequest(views.html.ingest.curated(form, e.getMessage, e.target))
        case e : IngestException =>
          Logger.error("curated ingest error", e)
          BadRequest(views.html.ingest.curated(form, e.getMessage))
      }
  }

  final class AdolphForm(implicit request : VolumeController.Request[_])
    extends HtmlForm[AdolphForm](
      routes.IngestController.adolph(request.obj.id),
      views.html.ingest.adolph(_))
    with CSVForm {
    val sessions = CSV()
    val participants = CSV(OptionalFile())
    val run = Field(Forms.boolean)
  }

  def adolphView(v : models.Volume.Id) = Action(v).async { implicit request =>
    new AdolphForm().Ok
  }

  def adolph(v : models.Volume.Id) = Action(v).async { implicit request =>
    val volume = request.obj
    val form = new AdolphForm()._bind
    if (!form.run.get)
      ingest.Adolph.parse(form.sessions.get.ref.file, form.participants.get.map(_.ref.file)).map { r =>
        Ok(views.html.ingest.adolph(form, r.toString + " records found"))
      }.recover {
        case e : IngestException =>
          BadRequest(views.html.ingest.adolph(form, e.getMessage))
      }
    else 
      ingest.Adolph.process(volume, form.sessions.get.ref.file, form.participants.get.map(_.ref.file)).map { r =>
        Ok(views.html.ingest.result(volume, r))
      }.recover {
        case e : PopulateException =>
          Logger.error("adolph ingest error", e)
          BadRequest(views.html.ingest.adolph(form, e.getMessage, e.target))
        case e : IngestException =>
          Logger.error("adolph ingest error", e)
          BadRequest(views.html.ingest.adolph(form, e.getMessage))
      }
  }

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
        async.void)
      .map(r => Ok(views.html.ingest.json(form, r.toString)))
    }.recover(PartialFunction { e =>
      BadRequest(views.html.ingest.json(form, e.getMessage))
    })
  }

}
