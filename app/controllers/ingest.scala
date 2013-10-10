package controllers

import site._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import scala.concurrent.Future.successful
import models._
import ingest._

object Ingest extends SiteController {
  private def Action(i : models.Volume.Id) =
    SiteAction.access(Permission.ADMIN) ~> Volume.action(i, Permission.EDIT)

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
      successful(BadRequest(views.html.ingest.csv(volume, form, text)))
    form.fold(
      form => bad(form),
      { case ((), run) => request.body.asMultipartFormData.flatMap(_.file("file")).fold(
        bad(form.withError("file", "error.required"))
      ) { file => AssetFormat.getFilePart(file).map(_.mimetype).orElse(file.contentType) match
        { case Some("text/csv") if !run => try {
            successful(Ok(views.html.ingest.csv(volume, form, ingest.Curated.preview(file.ref.file))))
          } catch { case e : IngestException =>
            bad(form, e.getMessage)
          }
          case Some("text/csv") if run => request.futureDB { implicit request => try {
            Ok(views.html.ingest.curated(volume, ingest.Curated.populate(file.ref.file, volume)))
          } catch {
            case e : ingest.Curated.PopulateException =>
              BadRequest(views.html.ingest.csv(volume, form, e.getMessage, e.target))
            case e : IngestException =>
              BadRequest(views.html.ingest.csv(volume, form, e.getMessage))
          }
          }(site.context.process)
          case f =>
            bad(form.withError("file", "file.format.unknown", f.fold("unknown")(_.toString)))
        } }
      }
    )
  }

}
