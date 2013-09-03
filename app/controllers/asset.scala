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
    Ok(views.html.asset.link(link))
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

  type AssetForm = Form[(String, String, Option[(Classification.Value, Unit)])]
  private[this] def assetForm(file : Boolean) : AssetForm = Form(tuple(
    "title" -> nonEmptyText,
    "description" -> text,
    "" -> MaybeMapping(Some(tuple(
      "classification" -> Field.enum(Classification),
      "file" -> ignored(()))).filter(_ => file))
  ))

  private[this] def formFill(link : AssetLink)(implicit site : Site) : AssetForm = {
    /* Under what conditions should FileAsset data be allowed to be changed? */
    assetForm(false).fill((link.title, link.description.getOrElse(""), None))
    }

  /* FIXME this doesn't work in error cases */
  def formForFile(form : AssetForm) = form.value.fold(false)(!_._3.isEmpty)

  def edit(s : models.Container.Id, o : models.Asset.Id) = check(s, o, Permission.EDIT) { link => implicit request =>
    Ok(views.html.asset.edit(Right(link), formFill(link)))
  }

  def change(s : models.Container.Id, o : models.Asset.Id) = check(s, o, Permission.EDIT) { link => implicit request =>
    formFill(link).bindFromRequest.fold(
      form => BadRequest(views.html.asset.edit(Right(link), form)), {
      case (title, description, file) =>
        link.change(title = title, description = maybe(description))
        /* file foreach {
          () => link.asset.asInstanceOf[models.FileAsset].change
        } */
        Redirect(link.pageURL)
      }
    )
  }

  private[this] val uploadForm = assetForm(true)

  def create(c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { container => implicit request =>
    Ok(views.html.asset.edit(Left(container), uploadForm))
  }

  def upload(c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { container => implicit request =>
    val form = uploadForm.bindFromRequest
    val file = request.body.asMultipartFormData.flatMap(_.file("file"))
    (if (file.isEmpty) form.withError("file", "error.required") else form).fold(
      form => BadRequest(views.html.asset.edit(Left(container), form)), {
      case (title, description, fileData) =>
        val (classification, ()) = fileData.get
        val f = file.get
        f.contentType.flatMap(AssetFormat.getMimetype(_)).fold(
          BadRequest(views.html.asset.edit(Left(container), form.withError("file", "file.format.unknown", f.contentType.getOrElse("unknown")))) : Result
        ) { format =>
          val asset = models.FileAsset.create(format, classification, f.ref)
          val link = AssetLink.create(container, asset, maybe(title).getOrElse(f.filename), maybe(description))
          Redirect(link.pageURL)
        }
      }
    )
  }

}
