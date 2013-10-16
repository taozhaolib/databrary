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
import site._
import models._
import dbrary.Offset

object Asset extends SiteController {
  type ContainerRequest[A] = RequestObject[ContainerAsset]#Site[A]
  type SlotRequest[A] = RequestObject[SlotAsset]#Site[A]

  private[controllers] def containerAction(v : models.Volume.Id, i : models.Container.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(v, models.ContainerAsset.get(a, i)(_), p)

  private[controllers] def ContainerAction(v : models.Volume.Id, i : models.Container.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> containerAction(v, i, a, p)

  private[controllers] def slotAction(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(v, models.SlotAsset.get(a, i)(_), p)

  private[controllers] def SlotAction(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> slotAction(v, i, a, p)

  def view(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id) = SlotAction(v, i, a) { implicit request =>
    Ok(views.html.asset.view(request.obj))
  }

  private def assetResult(tag : String, data_ : => Future[store.StreamEnumerator], fmt : AssetFormat, saveAs : Option[String])(request : Request[_]) : Future[SimpleResult] = {
    /* The split works because we never use commas within etags. */
    val ifNoneMatch = request.headers.getAll(IF_NONE_MATCH).flatMap(_.split(',').map(_.trim))
    /* Assuming assets are immutable, any if-modified-since header is good enough */
    if (ifNoneMatch.exists(t => t.equals("*") || HTTP.unquote(t).equals(tag)) ||
      ifNoneMatch.isEmpty && request.headers.get(IF_MODIFIED_SINCE).isDefined)
      Future.successful(NotModified)
    else data_.map { data =>
      val size = data.size
      val range = if (request.headers.get(IF_RANGE).forall(HTTP.unquote(_).equals(tag)))
          request.headers.get(RANGE).flatMap(HTTP.parseRange(_, size))
        else
          None
      val subdata = range.fold(data)((data.range _).tupled)
      val headers = Seq[Option[(String, String)]](
        Some(CONTENT_LENGTH -> subdata.size.toString),
        range.map(r => CONTENT_RANGE -> ("bytes " + (if (r._1 >= size) "*" else r._1.toString + "-" + r._2.toString) + "/" + data.size.toString)),
        Some(CONTENT_TYPE -> fmt.mimetype),
        saveAs.map(name => CONTENT_DISPOSITION -> ("attachment; filename=" + HTTP.quote(name + fmt.extension.fold("")("." + _)))),
        Some(ETAG -> HTTP.quote(tag)),
        Some(CACHE_CONTROL -> "max-age=31556926, private") /* this needn't be private for public data */
      ).flatten
        SimpleResult(
          header = ResponseHeader(range.fold(OK)(r => if (r._1 >= size) REQUESTED_RANGE_NOT_SATISFIABLE else PARTIAL_CONTENT),
            Map(headers : _*)),
          body = subdata)
      }
  }

  def download(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, inline : Boolean) = SlotAction(v, i, o, Permission.DOWNLOAD).async { implicit request =>
    assetResult(
      "sobj:%d:%d".format(request.obj.slotId.unId, request.obj.link.assetId.unId),
      store.Asset.read(request.obj),
      request.obj.format,
      if (inline) None else Some(request.obj.link.name)
    )(request)
  }

  private def getFrame(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, offset : Either[Float,Offset]) = SlotAction(v, i, o, Permission.DOWNLOAD).async { implicit request =>
    request.obj match {
      case ts : SlotTimeseries =>
        val off = offset.fold(f => Offset(10*(f*ts.duration.seconds/10).floor), identity)
        if (off < 0 || off >= ts.duration)
          Future.successful(NotFound)
        else assetResult(
          "sframe:%d:%d:%d".format(ts.slotId.unId, ts.link.assetId.unId, off.millis.toLong),
          store.Asset.readFrame(ts, off),
          ts.source.format.sampleFormat,
          None
        )(request)
      case _ => Future.successful(NotFound)
    }
  }
  def frame(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, eo : Offset) = getFrame(v, i, o, Right(eo))
  def head(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id) = getFrame(v, i, o, Right(0))
  def thumb(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id) = getFrame(v, i, o, Left(0.25f))

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

  private[this] def formFill(implicit request : ContainerRequest[_]) : AssetForm = {
    /* TODO Under what conditions should FileAsset data be allowed to be changed? */
    assetForm(false).fill((request.obj.name, request.obj.body.getOrElse(""), request.obj.position, None))
  }

  def formForFile(form : AssetForm, target : Either[Container,ContainerAsset]) =
    form.value.fold(target.isLeft)(_._4.isDefined)

  def edit(v : models.Volume.Id, s : models.Container.Id, o : models.Asset.Id) = ContainerAction(v, s, o, Permission.EDIT) { implicit request =>
    Ok(views.html.asset.edit(Right(request.obj), formFill))
  }

  def change(v : models.Volume.Id, s : models.Container.Id, o : models.Asset.Id) = ContainerAction(v, s, o, Permission.EDIT) { implicit request =>
    formFill.bindFromRequest.fold(
      form => BadRequest(views.html.asset.edit(Right(request.obj), form)), {
      case (name, body, position, file) =>
        request.obj.change(name = name, body = maybe(body), position = position)
        /* file foreach {
          () => request.obj.asset.asInstanceOf[models.FileAsset].change
        } */
        Redirect(request.obj.container.fullSlot.pageURL)
      }
    )
  }

  private[this] val uploadForm = assetForm(true)

  def create(v : models.Volume.Id, c : models.Container.Id, offset : Option[Offset]) = Container.Action(v, c, Permission.CONTRIBUTE) { implicit request =>
    Ok(views.html.asset.edit(Left(request.obj), uploadForm.fill(("", "", offset, Some((None, Classification.IDENTIFIED, None, ()))))))
  }

  def upload(v : models.Volume.Id, c : models.Container.Id) = Container.Action(v, c, Permission.CONTRIBUTE) { implicit request =>
    def error(form : AssetForm) : SimpleResult =
      BadRequest(views.html.asset.edit(Left(request.obj), form))
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
              (fmt orElse AssetFormat.getFilename(name, ts)).fold[ER](Left(form.withError("format", "Unknown format")))(
                fmt => Right((store.TemporaryFileCopy(file), fmt, name)))
            else
              Left(form.withError("localfile", "File not found"))
          }
        } { file =>
          (fmt orElse AssetFormat.getFilePart(file, ts)).fold[ER](
            Left(form.withError("file", "file.format.unknown", file.contentType.getOrElse("unknown"))))(
            fmt => Right((file.ref, fmt, file.filename)))
        }.fold(error _, {
          case (file, fmt, fname) =>
            val asset = fmt match {
              case fmt : TimeseriesFormat if ts => // "if ts" should be redundant
                val probe = media.AV.probe(file.file)
                models.Timeseries.create(fmt, classification, probe.duration, file)
              case _ =>
                models.FileAsset.create(fmt, classification, file)
            }
            val link = ContainerAsset.create(request.obj, asset, position, maybe(name).getOrElse(fname), maybe(body))
            Redirect(routes.Asset.view(link.volumeId, link.container.fullSlot.id, link.asset.id))
        })
      case _ => error(uploadForm) /* should not happen */
    })
  }

  def remove(v : models.Volume.Id, c : models.Container.Id, a : models.Asset.Id) = ContainerAction(v, c, a, Permission.EDIT) { implicit request =>
    request.obj.remove
    Redirect(request.obj.container.fullSlot.pageURL)
  }
}
