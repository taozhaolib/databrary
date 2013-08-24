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
import dbrary.Offset

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

  def frame(i : models.Container.Id, o : models.Asset.Id, offset : Offset = 0) = check(i, o, Permission.DOWNLOAD) { link => implicit request =>
    dbrary.cast[TimeseriesData](link.asset).filter(offset >= 0 && offset < _.duration).fold(NotFound : Result) { ts =>
      assetResult(
        "frame:%d:%f".format(link.assetId.unId, offset.seconds),
        store.Asset.readFrame(ts, offset),
        AssetFormat.Image,
        None
      )
    }
  }
  def head(i : models.Container.Id, o : models.Asset.Id) = frame(i, o)

  /* Any editable fields on FileAssets (currently none) */
  type FileFields = Unit
  private[this] val fileFields : Mapping[FileFields] = EmptyMapping

  type EditForm = Form[(String, String, Option[FileFields])]
  private[this] def formFill(link : AssetLink)(implicit site : Site) : EditForm = {
    /* Under what conditions should FileAsset data be allowed to be changed? */
    val file = link.asset match {
      case f : FileAsset => Some(f)
      case _ => None
    }
    Form(tuple(
      "title" -> nonEmptyText,
      "description" -> text,
      "" -> MaybeMapping(file.map(_ => fileFields))
    )).fill((link.title, link.description.getOrElse(""), file.map(f => ())))
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
        /* file foreach {
          () => link.asset.asInstanceOf[models.FileAsset].change
        } */
        Redirect(link.pageURL)
      }
    )
  }

  type UploadForm = Form[(String, String, Classification.Value, Unit)]
  private[this] val uploadForm = Form(tuple(
    "title" -> text,
    "description" -> text,
    "classification" -> Field.enum(Classification),
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
      case (title, description, classification, ()) =>
        val f = file.get
        f.contentType.flatMap(AssetFormat.getMimetype(_)).fold(
          BadRequest(views.html.assetCreate(container, form.withError("file", "file.format.unknown", f.contentType.getOrElse("unknown")))) : Result)
        { format =>
          val asset = models.FileAsset.create(format, classification, f.ref)
          val link = AssetLink.create(container, asset, maybe(title).getOrElse(f.filename), maybe(description))
          Redirect(link.pageURL)
        }
      }
    )
  }

}
