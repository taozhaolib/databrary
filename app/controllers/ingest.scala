package controllers

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import site._
import models._
import ingest._

object Ingest extends SiteController {
  private def Action(i : models.Volume.Id) =
    SiteAction.rootAccess(Permission.ADMIN) ~> VolumeController.action(i, Permission.EDIT)

  type CSVForm = Form[(Unit, Boolean)]
  private val csvForm : CSVForm = Form(tuple(
    "file" -> ignored(()),
    "run" -> boolean
  ))

  def csv(v : models.Volume.Id) = Action(v) { implicit request =>
    Ok(views.html.ingest.csv(request.obj, csvForm))
  }

  def curated(v : models.Volume.Id) = Action(v).async { implicit request =>
    val volume = request.obj
    val form = csvForm.bindFromRequest
    def bad(form : CSVForm, text : String = "") =
      ABadRequest(views.html.ingest.csv(volume, form, text))
    form.fold(
      form => bad(form),
      { case ((), run) => request.body.asMultipartFormData.flatMap(_.file("file")).fold(
        bad(form.withError("file", "error.required"))
      ) { file => AssetFormat.getFilePart(file).map(_.mimetype).orElse(file.contentType) match
        { case Some("text/csv") if !run =>
            Future(ingest.Curated.preview(file.ref.file))(site.context.process).map { r =>
              Ok(views.html.ingest.csv(volume, form, r))
            }.recover {
              case e : IngestException =>
                BadRequest(views.html.ingest.csv(volume, form, e.getMessage))
            }
          case Some("text/csv") if run =>
            ingest.Curated.populate(file.ref.file, volume).map { r =>
              Ok(views.html.ingest.curated(volume, r))
            }.recover {
              case e : ingest.Curated.PopulateException =>
                BadRequest(views.html.ingest.csv(volume, form, e.getMessage, e.target))
              case e : IngestException =>
                BadRequest(views.html.ingest.csv(volume, form, e.getMessage))
            }
          case f =>
            bad(form.withError("file", "file.format.unknown", f.fold("unknown")(_.toString)))
        }
      } }
    )
  }

}
