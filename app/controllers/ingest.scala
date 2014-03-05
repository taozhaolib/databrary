package controllers

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import site._
import models._
import ingest._

object IngestController extends SiteController {
  private def Action(i : models.Volume.Id) =
    SiteAction.rootAccess(Permission.ADMIN) ~> VolumeController.action(i, Permission.EDIT)

  trait CSVForm extends StructForm {
    protected def CSVFile() : File = File()
      .verifying(validation.Constraint[FilePart] { (f : FilePart) =>
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
    val file = CSVFile()
    val run = Field(Forms.boolean)
  }

  def curatedView(v : models.Volume.Id) = Action(v).async { implicit request =>
    new CuratedForm().Ok
  }

  def curated(v : models.Volume.Id) = Action(v).async { implicit request =>
    val volume = request.obj
    val form = new CuratedForm()._bind
    if (!form.run.get)
      Future(ingest.Curated.preview(form.file.get.ref.file))(site.context.process).map { r =>
	Ok(views.html.ingest.curated(form, r))
      }.recover {
	case e : IngestException =>
	  BadRequest(views.html.ingest.curated(form, e.getMessage))
      }
    else 
      ingest.Curated.populate(form.file.get.ref.file, volume).map { r =>
	Ok(views.html.ingest.result(volume, r._1 ++ r._2))
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
    val sessions = CSVFile()
    val participants = CSVFile()
    val run = Field(Forms.boolean)
  }

  def adolphView(v : models.Volume.Id) = Action(v).async { implicit request =>
    new AdolphForm().Ok
  }

  def adolph(v : models.Volume.Id) = Action(v).async { implicit request =>
    val volume = request.obj
    val form = new AdolphForm()._bind
    if (!form.run.get)
      ingest.Adolph.parse(form.sessions.get.ref.file, form.participants.get.ref.file).map { r =>
	Ok(views.html.ingest.adolph(form, r.toString + " sessions found"))
      }.recover {
	case e : IngestException =>
	  BadRequest(views.html.ingest.adolph(form, e.getMessage))
      }
    else 
      ingest.Adolph.process(volume, form.sessions.get.ref.file, form.participants.get.ref.file).map { r =>
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

}
