package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import          libs.iteratee.Enumerator
import          libs.concurrent.Execution.Implicits.defaultContext
import util._
import models._

object Asset extends SiteController {

  private[controllers] def check(i : models.Container.Id, o : models.Asset.Id, p : Permission.Value = Permission.VIEW)(act : AssetLink => SiteRequest[AnyContent] => Result) = Container.check(i) { container => implicit request =>
    container.getAsset(o).fold(NotFound : Result) { link =>
      if (link.permission < p)
        Forbidden
      else
        act(link)(request)
    }
  }

  def view(i : models.Container.Id, o : models.Asset.Id) = check(i, o) { link => implicit request =>
    Ok(views.html.assetLink(link))
  }

  private def assetResult(tag : String, data_ : => Future[store.StreamEnumerator], fmt : AssetFormat, saveAs : Option[String])(implicit request : SiteRequest[_]) : Result =
    /* Assuming assets are immutable, any if-modified-since header is good enough */
    request.headers.get(IF_NONE_MATCH).filter(_ == tag).orElse(
      request.headers.get(IF_MODIFIED_SINCE)
    ).fold(AsyncResult(data_.map { data =>
      val headers = Seq[Option[(String, String)]](
        data.size.map(CONTENT_LENGTH -> _.toString),
        Some(CONTENT_TYPE -> fmt.mimetype),
        saveAs.map(name => CONTENT_DISPOSITION -> ("attachment; filename=\"" + (name + fmt.extension.fold("")("." + _)).replaceAll("([\\p{Cntrl}\"\\\\])", "\\\\$2") + "\"")),
        Some(ETAG -> tag),
        Some(CACHE_CONTROL -> "max-age=31556926, private") /* this needn't be private for public data */
      ).flatten
      SimpleResult(
        header = ResponseHeader(OK, Map(headers : _*)),
        data)
    }) : Result) (_ => NotModified)
    
  def download(i : models.Container.Id, o : models.Asset.Id, inline : Boolean) = check(i, o, Permission.DOWNLOAD) { link => implicit request =>
    assetResult(
      link.assetId.unId.formatted("obj:%d"),
      store.Asset.read(link.asset),
      link.asset.format,
      if (inline) None else Some(link.title)
    )
  }

  def frame(i : models.Container.Id, o : models.Asset.Id, offset : dbrary.Interval = dbrary.Interval(0)) = check(i, o, Permission.DOWNLOAD) { link => implicit request =>
    assetResult(
      "frame:%d:%f".format(link.assetId.unId, offset.seconds),
      store.Asset.readFrame(link.asset, offset),
      FileFormat.Image,
      None
    )
  }
  def head(i : models.Container.Id, o : models.Asset.Id) = frame(i, o)

  private[this] val fileFields = tuple(
    "consent" -> form.enumField(Consent),
    "date" -> optional(sqlDate)
  )

  type EditForm = Form[(String, String, Option[(Consent.Value, Option[java.sql.Date])])]
  private[this] def formFill(link : AssetLink)(implicit site : Site) : EditForm = {
    /* Only allow file parameters to be changed if this is the original study for this asset */
    val file = link.asset match {
      case f : FileAsset if f.ownerId.fold(false)(_ == link.container.studyId) => Some(f)
      case _ => None
    }
    Form(tuple(
      "title" -> nonEmptyText,
      "description" -> text,
      "" -> MaybeMapping(file.map(_ => fileFields))
    )).fill((link.title, link.description.getOrElse(""), file.map(f => (f.consent, f.date))))
  }

  def formForFile(form : EditForm) = form.value.fold(false)(!_._3.isEmpty)

  def edit(s : models.Container.Id, o : models.Asset.Id) = check(s, o, Permission.EDIT) { link => implicit request =>
    Ok(views.html.assetEdit(link, formFill(link)))
  }

  def change(s : models.Container.Id, o : models.Asset.Id) = check(s, o, Permission.EDIT) { link => implicit request =>
    formFill(link).bindFromRequest.fold(
      form => BadRequest(views.html.assetEdit(link, form)), {
      case (title, description, file) =>
        link.change(title = title, description = maybe(description))
        file foreach {
          case (consent, date) => link.asset.asInstanceOf[models.FileAsset].change(consent = consent, date = date)
        }
        Redirect(link.pageURL)
      }
    )
  }

  type UploadForm = Form[(String, String, (Consent.Value, Option[java.sql.Date]), Unit)]
  private[this] val uploadForm = Form(tuple(
    "title" -> text,
    "description" -> text,
    "" -> fileFields,
    "file" -> ignored(())
  ))

  def create(c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { container => implicit request =>
    Ok(views.html.assetCreate(container, uploadForm))
  }

  def upload(c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { container => implicit request =>
    val form = uploadForm.bindFromRequest
    val file = request.body.asMultipartFormData.flatMap(_.file("file"))
    (if (file.isEmpty) form.withError("file", "error.required") else form).fold(
      form => BadRequest(views.html.assetCreate(container, form)), {
      case (title, description, (consent, date), ()) =>
        val f = file.get
        f.contentType.flatMap(AssetFormat.getMimetype(_)).fold(
          BadRequest(views.html.assetCreate(container, form.withError("file", "file.format.unknown", f.contentType.getOrElse("unknown")))) : Result)
        { format =>
          val asset = models.Asset.create(format, container.studyId, consent, date, f.ref)
          val link = AssetLink.create(container, asset, maybe(title).getOrElse(f.filename), maybe(description))
          Redirect(link.pageURL)
        }
      }
    )
  }

}
