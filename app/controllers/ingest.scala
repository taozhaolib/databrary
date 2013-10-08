package controllers

import site._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
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

  def curated(v : models.Volume.Id) = Action(v) { implicit request =>
    val form = csvForm.bindFromRequest
    form.fold(
      form => BadRequest(views.html.ingest.csv(request.obj, form)),
      { case ((), run) => request.body.asMultipartFormData.flatMap(_.file("file")).fold(
        BadRequest(views.html.ingest.csv(request.obj, form.withError("file", "error.required")))
      ) { file => AssetFormat.getFilePart(file).map(_.mimetype).orElse(file.contentType) match
        { case Some("text/csv") if !run => try {
            Ok(views.html.ingest.csv(request.obj, form, ingest.Curated.preview(file.ref.file)))
          } catch { case e : IngestException =>
            BadRequest(views.html.ingest.csv(request.obj, form, e.getMessage))
          }
          case Some("text/csv") if run => try {
            Ok(views.html.ingest.curated(request.obj, ingest.Curated.populate(file.ref.file, request.obj)))
          } catch {
            case e : ingest.Curated.PopulateException =>
              BadRequest(views.html.ingest.csv(request.obj, form, e.getMessage, e.target))
            case e : IngestException =>
              BadRequest(views.html.ingest.csv(request.obj, form, e.getMessage))
          }
          case f =>
            BadRequest(views.html.ingest.csv(request.obj, form.withError("file", "file.format.unknown", f.fold("unknown")(_.toString))))
        } }
      }
    )
  }

}
