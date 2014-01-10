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

private[controllers] sealed class AssetController extends ObjectController[Asset] {
  protected def action(i : models.Asset.Id, p : Permission.Value) =
    RequestObject.check(models.Asset.get(i)(_), p)

  protected def Action(i : models.Asset.Id, p : Permission.Value) =
    SiteAction ~> action(i, p)

  type AssetMapping = (
    Option[String], 
    Option[Classification.Value], 
    Option[(
      Container.Id, 
      Option[Offset]
    )], 
    Option[(
      Option[AssetFormat.Id], 
      Boolean, 
      Option[String], Unit)
    ]
  )
  type AssetForm = Form[AssetMapping]
  protected def assetForm(file : Boolean) : AssetForm = Form(tuple(
    "name" -> OptionMapping(text),
    "classification" -> OptionMapping(Field.enum(Classification)),
    "" -> optional(tuple(
      "container" -> of[Container.Id],
      "position" -> optional(of[Offset])
    )),
    "" -> MaybeMapping(if (file) Some(tuple(
      "format" -> optional(of[AssetFormat.Id]),
      "timeseries" -> boolean,
      "localfile" -> optional(nonEmptyText),
      "file" -> ignored(()))) else None)
  ))

  protected def formFill(implicit request : Request[_]) : AssetForm = {
    /* TODO Under what conditions should FileAsset data be allowed to be changed? */
    assetForm(false).fill((Some(request.obj.name.getOrElse("")), Some(request.obj.classification), None, None))
  }

  def formForFile(form : AssetForm, target : Either[Volume,Asset]) =
    form.value.fold(target.isLeft)(_._4.isDefined)

  def update(o : models.Asset.Id) = Action(o, Permission.EDIT).async { implicit request =>
    def bad(form : AssetForm) =
      new BadFormException[AssetMapping](views.html.asset.edit(Right(request.obj), _))(form)
    val form = formFill.bindFromRequest
    form.fold(bad(_).result, {
      case (name, classification, slot, _) => for {
          container <- macros.Async.map[Container.Id, Container](slot.map(_._1), Container.get(_).map(_.getOrElse(
            throw bad(form.withError("container", "Invalid container ID")))))
          _ <- request.obj.change(classification = classification, name = name.map(Maybe(_).opt))
          _ <- macros.Async.foreach[Container, Unit](container, request.obj.link(_, slot.flatMap(_._2)))
        } yield (result(request.obj))
      }
    )
  }

  private[controllers] def assetResult(asset : BackedAsset, saveAs : Option[String] = None)(implicit request : SiteRequest[_]) : Future[SimpleResult] = {
    val tag = asset.etag
    /* The split works because we never use commas within etags. */
    val ifNoneMatch = request.headers.getAll(IF_NONE_MATCH).flatMap(_.split(',').map(_.trim))
    /* Assuming assets are immutable, any if-modified-since header is good enough */
    if (ifNoneMatch.exists(t => t.equals("*") || HTTP.unquote(t).equals(tag)) ||
      ifNoneMatch.isEmpty && request.headers.get(IF_MODIFIED_SINCE).isDefined)
      macros.Async(NotModified)
    else for {
      data <- store.Asset.read(asset)
      date <- asset.source.creation
    } yield {
      val size = data.size
      val range = if (request.headers.get(IF_RANGE).forall(HTTP.unquote(_).equals(tag)))
          request.headers.get(RANGE).flatMap(HTTP.parseRange(_, size))
        else
          None
      val subdata = range.fold(data)((data.range _).tupled)
      val headers = Seq[Option[(String, String)]](
        Some(CONTENT_LENGTH -> subdata.size.toString),
        range.map(r => CONTENT_RANGE -> ("bytes " + (if (r._1 >= size) "*" else r._1.toString + "-" + r._2.toString) + "/" + data.size.toString)),
        Some(CONTENT_TYPE -> asset.format.mimetype),
        saveAs.map(name => CONTENT_DISPOSITION -> ("attachment; filename=" + HTTP.quote(name + asset.format.extension.fold("")("." + _)))),
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
}

object AssetController extends AssetController

object AssetHtml extends AssetController {
  def view(o : models.Asset.Id) = Action(o, Permission.VIEW).async { implicit request =>
    request.obj.slot.map(_.fold[SimpleResult](
      NotFound /* TODO */)(
      sa => Redirect(sa.in(sa.slot.container).pageURL)))
  }

  def edit(o : models.Asset.Id) = Action(o, Permission.EDIT) { implicit request =>
    Ok(views.html.asset.edit(Right(request.obj), formFill))
  }

  private[this] val uploadForm = assetForm(true)

  def create(v : models.Volume.Id, c : Option[Container.Id], pos : Option[Offset]) = VolumeHtml.Action(v, Permission.CONTRIBUTE) { implicit request =>
    Ok(views.html.asset.edit(Left(request.obj), uploadForm.fill((Some(""), Some(Classification.IDENTIFIED), c.map((_, pos)), Some((None, false, None, ()))))))
  }

  def upload(v : models.Volume.Id) = VolumeHtml.Action(v, Permission.CONTRIBUTE).async { implicit request =>
    val volume = request.obj
    def Error(form : AssetForm) =
      throw new BadFormException[AssetMapping](views.html.asset.edit(Left(request.obj), _))(form)
    val form = uploadForm.bindFromRequest
    form.fold(Error _, {
      case (name, classification, slot, Some((format, timeseries, localfile, ()))) =>
        val adm = request.access >= Permission.ADMIN
        val ifmt = format.filter(_ => adm).flatMap(AssetFormat.get(_))
        val (file, fmt, fname) =
          request.body.asMultipartFormData.flatMap(_.file("file")).fold {
            /* local file handling, for admin only: */
            val file = new java.io.File(localfile.filter(_ => adm) getOrElse
              Error(form.withError("file", "error.required")))
            val name = file.getName
            if (!file.isFile)
              Error(form.withError("localfile", "File not found"))
            val ffmt = ifmt orElse AssetFormat.getFilename(name) getOrElse
              Error(form.withError("format", "Unknown format"))
            (store.TemporaryFileLinkOrCopy(file) : TemporaryFile, ffmt, name)
          } { file =>
            val ffmt = ifmt orElse AssetFormat.getFilePart(file) getOrElse
              Error(form.withError("file", "file.format.unknown", file.contentType.getOrElse("unknown")))
            (file.ref, ffmt, file.filename)
          }
        val aname = name.flatMap(Maybe(_).opt).orElse(Maybe(fname).opt)
        for {
          container <- macros.Async.map[Container.Id, Container](slot.map(_._1), Container.get(_).map(_ getOrElse
            Error(form.withError("container", "Invalid container ID"))))
          asset <- fmt match {
            case fmt : TimeseriesFormat if adm && timeseries =>
              val probe = media.AV.probe(file.file)
              models.Asset.create(volume, fmt, classification.getOrElse(Classification(0)), probe.duration, aname, file)
            case _ =>
              models.Asset.create(volume, fmt, classification.getOrElse(Classification(0)), aname, file)
          }
          sa <- macros.Async.map[Container, SlotAsset](container, asset.link(_, slot.flatMap(_._2)))
        } yield (Redirect(sa.getOrElse(asset).pageURL))
      case _ => Error(uploadForm) /* should not happen */
    })
  }

  def remove(a : models.Asset.Id) = Action(a, Permission.EDIT).async { implicit request =>
    for {
      _ <- request.obj.unlink
    } yield (Redirect(request.obj.pageURL))
  }
}

object AssetApi extends AssetController {
  def get(i : models.Asset.Id) = Action(i, Permission.VIEW).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }
}
