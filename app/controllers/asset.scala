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

  private[controllers] def checkContainer(i : models.Container.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW)(act : ContainerAsset => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    ContainerAsset.get(a, i).fold(NotFound : Result) { link =>
      if (link.permission < p)
        Forbidden
      else
        act(link)(request)
    }
  }

  private[controllers] def checkSlot(i : models.Slot.Id, o : models.Asset.Id, p : Permission.Value = Permission.VIEW)(act : SlotAsset => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    SlotAsset.get(o, i).fold(NotFound : Result) { link =>
      if (link.permission < p)
        Forbidden
      else
        act(link)(request)
    }
  }

  def view(i : models.Slot.Id, a : models.Asset.Id) = checkSlot(i, a) { link => implicit request =>
    Ok(views.html.asset.view(link))
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

  def download(i : models.Slot.Id, o : models.Asset.Id, inline : Boolean) = checkSlot(i, o, Permission.DOWNLOAD) { link => implicit request =>
    assetResult(
      "sobj:%d:%d".format(link.slotId.unId, link.link.assetId.unId),
      store.Asset.read(link),
      link.link.asset.format,
      if (inline) None else Some(link.link.name)
    )
  }

  def frame(i : models.Slot.Id, o : models.Asset.Id, offset : Offset = 0) = checkSlot(i, o, Permission.DOWNLOAD) { link => implicit request =>
    link match {
      case ts : SlotTimeseries if offset >= 0 && offset < ts.duration =>
        assetResult(
          "sframe:%d:%d:%d".format(link.slotId.unId, link.link.assetId.unId, offset.millis.toLong),
          store.Asset.readFrame(ts, offset),
          ts.source.format.sampleFormat,
          None
        )
      case _ => NotFound
    }
  }
  def head(i : models.Slot.Id, o : models.Asset.Id) = frame(i, o)

  type AssetForm = Form[(String, String, Option[Offset], Option[(Classification.Value, Unit)])]
  private[this] def assetForm(file : Boolean) : AssetForm = Form(tuple(
    "name" -> nonEmptyText,
    "body" -> text,
    "offset" -> optional(Field.offset),
    "" -> MaybeMapping(Some(tuple(
      "classification" -> Field.enum(Classification),
      "file" -> ignored(()))).filter(_ => file))
  ))

  private[this] def formFill(link : ContainerAsset)(implicit site : Site) : AssetForm = {
    /* TODO Under what conditions should FileAsset data be allowed to be changed? */
    assetForm(false).fill((link.name, link.body.getOrElse(""), link.offset, None))
  }

  /* FIXME this doesn't work in error cases */
  def formForFile(form : AssetForm) = form.value.fold(false)(!_._3.isEmpty)

  def edit(s : models.Container.Id, o : models.Asset.Id) = checkContainer(s, o, Permission.EDIT) { link => implicit request =>
    Ok(views.html.asset.edit(Right(link), formFill(link)))
  }

  def change(s : models.Container.Id, o : models.Asset.Id) = checkContainer(s, o, Permission.EDIT) { link => implicit request =>
    formFill(link).bindFromRequest.fold(
      form => BadRequest(views.html.asset.edit(Right(link), form)), {
      case (name, body, offset, file) =>
        link.change(name = name, body = maybe(body), offset = offset)
        /* file foreach {
          () => link.asset.asInstanceOf[models.FileAsset].change
        } */
        Redirect(link.container.pageURL)
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
      case (name, body, offset, fileData) =>
        val (classification, ()) = fileData.get
        val f = file.get
        f.contentType.flatMap(AssetFormat.getMimetype(_)).fold(
          BadRequest(views.html.asset.edit(Left(container), form.withError("file", "file.format.unknown", f.contentType.getOrElse("unknown")))) : Result
        ) { format =>
          val asset = models.FileAsset.create(format, classification, f.ref)
          val link = ContainerAsset.create(container, asset, offset, maybe(name).getOrElse(f.filename), maybe(body))
          Redirect(link.container.pageURL)
        }
      }
    )
  }

}
