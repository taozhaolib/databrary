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

  final class CSVForm(implicit request : VolumeController.Request[_])
    extends HtmlForm[CSVForm](
      routes.IngestController.curated(request.obj.id),
      views.html.ingest.csv(_)) {
    val file = File()
    val run = Field(Forms.boolean)
  }

  def csv(v : models.Volume.Id) = Action(v).async { implicit request =>
    new CSVForm().Ok
  }

  def curated(v : models.Volume.Id) = Action(v).async { implicit request =>
    val volume = request.obj
    val form = new CSVForm()._bind
    val fmt = AssetFormat.getFilePart(form.file.get).map(_.mimetype).orElse(form.file.get.contentType)
    if (!fmt.exists(_.equals("text/csv")))
      form.file.withError("file.format.unknown", fmt.getOrElse("unknown"))._throw
    if (!form.run.get)
      Future(ingest.Curated.preview(form.file.get.ref.file))(site.context.process).map { r =>
	Ok(views.html.ingest.csv(form, r))
      }.recover {
	case e : IngestException =>
	  BadRequest(views.html.ingest.csv(form, e.getMessage))
      }
    else 
      ingest.Curated.populate(form.file.get.ref.file, volume).map { r =>
	Ok(views.html.ingest.curated(volume, r))
      }.recover {
	case e : PopulateException =>
	  BadRequest(views.html.ingest.csv(form, e.getMessage, e.target))
	case e : IngestException =>
	  BadRequest(views.html.ingest.csv(form, e.getMessage))
      }
  }

}
