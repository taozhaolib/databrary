package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import          libs.Files.TemporaryFile
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

  def view(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id) = checkSlot(i, a) { link => implicit request =>
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

  def download(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, inline : Boolean) = checkSlot(i, o, Permission.DOWNLOAD) { link => implicit request =>
    assetResult(
      "sobj:%d:%d".format(link.slotId.unId, link.link.assetId.unId),
      store.Asset.read(link),
      link.link.asset.format,
      if (inline) None else Some(link.link.name)
    )
  }

  def frame(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, offset : Offset = 0) = checkSlot(i, o, Permission.DOWNLOAD) { link => implicit request =>
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
  def head(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id) = frame(v, i, o)

  type AssetForm = Form[(String, String, Option[Offset], Option[(Option[AssetFormat.Id], Classification.Value, Option[String], Unit)])]
  private[this] def assetForm(file : Boolean) : AssetForm = Form(tuple(
    "name" -> nonEmptyText,
    "body" -> text,
    "offset" -> optional(Field.offset),
    "" -> MaybeMapping(if (file) Some(tuple(
      "format" -> optional(of[AssetFormat.Id]),
      "classification" -> Field.enum(Classification),
      "localfile" -> optional(nonEmptyText),
      "file" -> ignored(()))) else None)
  ))

  private[this] def formFill(link : ContainerAsset)(implicit site : Site) : AssetForm = {
    /* TODO Under what conditions should FileAsset data be allowed to be changed? */
    assetForm(false).fill((link.name, link.body.getOrElse(""), link.position, None))
  }

  /* FIXME this doesn't work in error cases */
  def formForFile(form : AssetForm) = form.value.fold(false)(_._4.isDefined)

  def edit(v : models.Volume.Id, s : models.Container.Id, o : models.Asset.Id) = checkContainer(s, o, Permission.EDIT) { link => implicit request =>
    Ok(views.html.asset.edit(Right(link), formFill(link)))
  }

  def change(v : models.Volume.Id, s : models.Container.Id, o : models.Asset.Id) = checkContainer(s, o, Permission.EDIT) { link => implicit request =>
    formFill(link).bindFromRequest.fold(
      form => BadRequest(views.html.asset.edit(Right(link), form)), {
      case (name, body, position, file) =>
        link.change(name = name, body = maybe(body), position = position)
        /* file foreach {
          () => link.asset.asInstanceOf[models.FileAsset].change
        } */
        Redirect(link.container.pageURL)
      }
    )
  }

  private[this] val uploadForm = assetForm(true).
    fill(("", "", None, Some((None, Classification.IDENTIFIED, None, ())))).
    ensuring(formForFile _)

  def create(v : models.Volume.Id, c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { container => implicit request =>
    Ok(views.html.asset.edit(Left(container), uploadForm))
  }

  def upload(v : models.Volume.Id, c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { container => implicit request =>
    def error(form : AssetForm) : Result =
      BadRequest(views.html.asset.edit(Left(container), form))
    val form = uploadForm.bindFromRequest
    uploadForm.bindFromRequest.fold(error _, {
      case (name, body, position, Some((format, classification, localfile, ()))) =>
        type ER = Either[AssetForm,(TemporaryFile,AssetFormat,String)]
        request.body.asMultipartFormData.flatMap(_.file("file")).fold {
          localfile.filter(_ => request.isAdmin).fold(
            Left(form.withError("file", "error.required")) : ER) { localfile =>
            val file = new java.io.File(localfile)
            val fmt = format.flatMap(AssetFormat.get(_))
            if (file.isFile)
              fmt.fold(Left(form.withError("format", "Invalid format")) : ER)(
                fmt => Right((TemporaryFile(file), fmt, file.getName)))
            else
              Left(form.withError("localfile", "File not found"))
          }
        } { file =>
          file.contentType.flatMap(AssetFormat.getMimetype(_)).fold(
            Left(form.withError("file", "file.format.unknown", file.contentType.getOrElse("unknown"))) : ER)(
            fmt => Right((file.ref, fmt, file.filename)))
        }.fold(error _, {
          case (file, fmt, fname) =>
            val asset = fmt match {
              case fmt : TimeseriesFormat =>
                val probe = media.AV.probe(file.file)
                models.Timeseries.create(fmt, classification, probe.duration, file)
              case _ =>
                models.FileAsset.create(fmt, classification, file)
            }
            val link = ContainerAsset.create(container, asset, position, maybe(name).getOrElse(fname), maybe(body))
            Redirect(link.container.pageURL)
        })
      case _ => error(uploadForm) /* should not happen */
      }
    )
  }

  def remove(v : models.Volume.Id, c : models.Container.Id, a : models.Asset.Id) = checkContainer(c, a, Permission.EDIT) { link => implicit request =>
    link.remove
    Redirect(link.container.pageURL)
  }
}
