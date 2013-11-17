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
import macros._
import dbrary._
import site._
import models._

object Asset extends SiteController {
  type Request[A] = RequestObject[SlotAsset]#Site[A]

  private[controllers] def action(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW, full : Boolean = false) =
    RequestObject.check(v, models.SlotAsset.get(a, i, full)(_), p)

  private[controllers] def Action(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW, full : Boolean = false) =
    SiteAction ~> action(v, i, a, p, full)

  def view(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id) = Action(v, i, a) { implicit request =>
    Ok(views.html.asset.view(request.obj))
  }

  private def assetResult(tag : String, data_ : => Future[store.StreamEnumerator], fmt : AssetFormat, saveAs : Option[String] = None)(implicit request : Request[_]) : Future[SimpleResult] = {
    val now = new Timestamp
    /* The split works because we never use commas within etags. */
    val ifNoneMatch = request.headers.getAll(IF_NONE_MATCH).flatMap(_.split(',').map(_.trim))
    /* Assuming assets are immutable, any if-modified-since header is good enough */
    if (ifNoneMatch.exists(t => t.equals("*") || HTTP.unquote(t).equals(tag)) ||
      ifNoneMatch.isEmpty && request.headers.get(IF_MODIFIED_SINCE).isDefined)
      macros.Async(NotModified)
    else for {
      data <- data_
      date <- request.obj.source.modification
    } yield {
      val size = data.size
      val range = if (request.headers.get(IF_RANGE).forall(HTTP.unquote(_).equals(tag)))
          request.headers.get(RANGE).flatMap(HTTP.parseRange(_, size))
        else
          None
      val subdata = range.fold(data)((data.range _).tupled)
      val headers = Seq[Option[(String, String)]](
        Some(DATE -> HTTP.date(now)),
        Some(CONTENT_LENGTH -> subdata.size.toString),
        range.map(r => CONTENT_RANGE -> ("bytes " + (if (r._1 >= size) "*" else r._1.toString + "-" + r._2.toString) + "/" + data.size.toString)),
        Some(CONTENT_TYPE -> fmt.mimetype),
        saveAs.map(name => CONTENT_DISPOSITION -> ("attachment; filename=" + HTTP.quote(name + fmt.extension.fold("")("." + _)))),
        date.map(d => (LAST_MODIFIED -> HTTP.date(d))),
        Some(ETAG -> HTTP.quote(tag)),
        Some(CACHE_CONTROL -> "max-age=31556926, private") /* this needn't be private for public data */
      ).flatten
        SimpleResult(
          header = ResponseHeader(range.fold(OK)(r => if (r._1 >= size) REQUESTED_RANGE_NOT_SATISFIABLE else PARTIAL_CONTENT),
            Map(headers : _*)),
          body = subdata)
      }
  }

  private def slotAssetResult(inline : Boolean = true)(implicit request : Request[_]) =
    assetResult(
      "sobj:%d:%d".format(request.obj.slotId.unId, request.obj.assetId.unId),
      store.Asset.read(request.obj),
      request.obj.format,
      if (inline) None else Some(request.obj.link.name)
    )

  def download(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, inline : Boolean) = Action(v, i, o, Permission.DOWNLOAD).async { implicit request =>
    slotAssetResult(inline)
  }

  private[controllers] def getFrame(offset : Either[Float,Offset])(implicit request : Request[_]) =
    request.obj match {
      case ts : SlotTimeseries =>
        val off = offset.fold(f => Offset(10*(f*ts.duration.seconds/10).floor), identity)
        if (off < 0 || off > ts.duration)
          Future.successful(NotFound)
        else assetResult(
          "sframe:%d:%d:%d".format(ts.slotId.unId, ts.link.assetId.unId, off.millis.toLong),
          store.Asset.readFrame(ts, off),
          ts.source.format.sampleFormat
        )
      case _ =>
        if (!offset.fold(_ => true, _ == 0))
          Future.successful(NotFound)
        else slotAssetResult()
    }
  def frame(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, eo : Offset) = Action(v, i, o, Permission.DOWNLOAD).async { implicit request =>
    getFrame(Right(eo))
  }
  def head(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id) =
    frame(v, i, o, 0)
  def thumb(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id) = Action(v, i, o, Permission.DOWNLOAD).async { implicit request =>
    getFrame(Left(0.25f))
  }

  type AssetForm = Form[(String, String, Option[Offset], Option[(Option[AssetFormat.Id], Classification.Value, Option[String], Unit)])]
  private[this] def assetForm(file : Boolean) : AssetForm = Form(tuple(
    "name" -> nonEmptyText,
    "body" -> text,
    "offset" -> optional(of[Offset]),
    "" -> MaybeMapping(if (file) Some(tuple(
      "format" -> optional(of[AssetFormat.Id]),
      "classification" -> Field.enum(Classification),
      "localfile" -> optional(nonEmptyText),
      "file" -> ignored(()))) else None)
  ))

  private[this] def formFill(implicit request : Request[_]) : AssetForm = {
    /* TODO Under what conditions should FileAsset data be allowed to be changed? */
    assetForm(false).fill((request.obj.link.name, request.obj.link.body.getOrElse(""), request.obj.link.position, None))
  }

  def formForFile(form : AssetForm, target : Either[Slot,SlotAsset]) =
    form.value.fold(target.isLeft)(_._4.isDefined)

  def edit(v : models.Volume.Id, s : models.Slot.Id, o : models.Asset.Id) = Action(v, s, o, Permission.EDIT, true) { implicit request =>
    Ok(views.html.asset.edit(Right(request.obj), formFill))
  }

  def change(v : models.Volume.Id, s : models.Slot.Id, o : models.Asset.Id) = Action(v, s, o, Permission.EDIT, true).async { implicit request =>
    formFill.bindFromRequest.fold(
      form => ABadRequest(views.html.asset.edit(Right(request.obj), form)), {
      case (name, body, position, file) => for {
          _ <- request.obj.link.change(name = name, body = Maybe(body).opt, position = position)
          /* file foreach {
            () => request.obj.asset.asInstanceOf[models.FileAsset].change
          } */
        } yield (Redirect(request.obj.slot.pageURL))
      }
    )
  }

  private[this] val uploadForm = assetForm(true)

  def create(v : models.Volume.Id, c : models.Slot.Id, offset : Option[Offset]) = Slot.Action(v, c, Permission.CONTRIBUTE, true) { implicit request =>
    Ok(views.html.asset.edit(Left(request.obj), uploadForm.fill(("", "", offset, Some((None, Classification.IDENTIFIED, None, ()))))))
  }

  def createTop(v : models.Volume.Id) = Volume.Action(v, Permission.CONTRIBUTE).async { implicit request =>
    request.obj.topSlot.map { slot =>
      Ok(views.html.asset.edit(Left(slot), uploadForm.fill(("", "", None, Some((None, Classification.MATERIAL, None, ()))))))
    }
  }

  def upload(v : models.Volume.Id, c : models.Slot.Id) = Slot.Action(v, c, Permission.CONTRIBUTE, true).async { implicit request =>
    def error(form : AssetForm) : Future[SimpleResult] =
      ABadRequest(views.html.asset.edit(Left(request.obj), form))
    val form = uploadForm.bindFromRequest
    form.fold(error _, {
      case (name, body, position, Some((format, classification, localfile, ()))) =>
        val ts = request.access >= Permission.ADMIN
        val fmt = format.filter(_ => ts).flatMap(AssetFormat.get(_, ts))
        type ER = Either[AssetForm,(TemporaryFile,AssetFormat,String)]
        request.body.asMultipartFormData.flatMap(_.file("file")).fold {
          localfile.filter(_ => ts).fold[ER](
            Left(form.withError("file", "error.required"))) { localfile =>
            /* local file handling, for admin only: */
            val file = new java.io.File(localfile)
            val name = file.getName
            if (file.isFile)
              (fmt orElse AssetFormat.getFilename(name, ts)).fold[ER](
                Left(form.withError("format", "Unknown format")))(
                fmt => Right((store.TemporaryFileCopy(file), fmt, name)))
            else
              Left(form.withError("localfile", "File not found"))
          }
        } { file =>
          (fmt orElse AssetFormat.getFilePart(file, ts)).fold[ER](
            Left(form.withError("file", "file.format.unknown", file.contentType.getOrElse("unknown"))))(
            fmt => Right((file.ref, fmt, file.filename)))
        }.fold(error _, { case (file, fmt, fname) =>
          for {
            asset <- fmt match {
              case fmt : TimeseriesFormat if ts => // "if ts" should be redundant
                val probe = media.AV.probe(file.file)
                models.Timeseries.create(fmt, classification, probe.duration, file)
              case _ =>
                models.FileAsset.create(fmt, classification, file)
            }
            link <- ContainerAsset.create(request.obj.container, asset, position, Maybe(name).orElse(fname), Maybe(body).opt)
            slot = link.container.fullSlot
          } yield (Redirect(routes.Asset.view(link.volumeId, slot.id, link.asset.id)))
        })
      case _ => error(uploadForm) /* should not happen */
    })
  }

  def remove(v : models.Volume.Id, c : models.Slot.Id, a : models.Asset.Id) = Action(v, c, a, Permission.EDIT, true).async { implicit request =>
    for {
      _ <- request.obj.link.remove
    } yield (Redirect(request.obj.slot.pageURL))
  }
}
