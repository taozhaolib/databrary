package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._
import ingest._

object Ingest extends SiteController {

  type CSVForm = Form[Unit]
  private val csvForm = Form(
    "file" -> ignored(())
  )

  def csv(v : models.Volume.Id) = AdminAction { implicit request =>
    Ok(views.html.ingest.csv(v, csvForm))
  }

  def preview(v : models.Volume.Id) = AdminAction { implicit request =>
    val form = csvForm.bindFromRequest
    request.body.asMultipartFormData.flatMap(_.file("file")).fold(
      BadRequest(views.html.ingest.csv(v, form.withError("file", "error.required")))
    ) { file => AssetFormat.getFilePart(file).map(_.mimetype).orElse(file.contentType) match
      { case Some("text/csv") => try {
          Ok(views.html.ingest.csv(v, form, ingest.Curated.preview(file.ref.file)))
        } catch { case e : IngestException =>
          BadRequest(views.html.ingest.csv(v, form, e.getMessage))
        }
        case f =>
          BadRequest(views.html.ingest.csv(v, form.withError("file", "file.format.unknown", f.fold("unknown")(_.toString))))
      }
    }
  }

}
