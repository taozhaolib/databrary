package controllers

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Result
import play.api.mvc.AnyContent
import macros._
import dbrary._
import models._

private[controllers] sealed class SlotAssetController extends ObjectController[SlotAsset] {
  private[controllers] def action(i : Container.Id, segment : Segment, a : Asset.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(models.SlotAsset.get(a, i, segment)(_), p)

  private[controllers] def Action(i : Container.Id, segment : Segment, a : Asset.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction andThen action(i, segment, a, p)
}

object SlotAssetController extends SlotAssetController {
  private[controllers] def getFrame(offset : Either[Float,Offset], size : Int)(implicit request : Request[AnyContent]) : Future[Result] = {
    val a = request.obj
    val s = a match {
      case ts : SlotTimeseries =>
        val off = offset.fold[Offset](f => Offset((f*ts.duration.millis).toLong), o => o)
        if (off < Offset.ZERO || off > ts.duration)
          throw NotFoundException
        ts.sample(off)
      case a =>
        if (!offset.fold(_ => true, _ == Offset.ZERO))
          throw NotFoundException
        a
    }
    if (s.format.isImage)
      AssetController.assetResult(s, Some(size.max(1).min(AssetController.defaultThumbSize)))
    else
      async(Found("/public/images/filetype/16px/" + a.format.extension.getOrElse("_blank") + ".png"))
  }

  def download(s : Container.Id, segment : Segment, o : models.Asset.Id, inline : Boolean) =
    Action(s, segment, o, Permission.READ).async { implicit request =>
      (if (inline) macros.async(None) else for {
        _ <- request.obj.auditDownload
        name <- request.obj.fileName
      } yield (Some(name)))
      .flatMap(AssetController.assetResult(request.obj, None, _))
    }

  def frame(i : Container.Id, o : Asset.Id, eo : Offset, size : Int = AssetController.defaultThumbSize) =
    head(i, Range.singleton(eo), o, size)
  def head(i : Container.Id, segment : Segment, o : models.Asset.Id, size : Int = AssetController.defaultThumbSize) = Action(i, segment, o, Permission.READ).async { implicit request =>
    getFrame(Right(Offset.ZERO), size)
  }
  def thumb(i : Container.Id, segment : Segment, o : models.Asset.Id, size : Int = AssetController.defaultThumbSize) = Action(i, segment, o, Permission.READ).async { implicit request =>
    getFrame(Left(0.25f), size)
  }
}

object SlotAssetHtml extends SlotAssetController with HtmlController {
  def view(i : Container.Id, segment : Segment, a : models.Asset.Id) = Action(i, segment, a).async { implicit request =>
    for {
      comments <- request.obj.comments
    } yield (Ok(views.html.asset.view(request.obj, comments)))
  }
}

object SlotAssetApi extends SlotAssetController with ApiController {
  def get(i : Container.Id, segment : Segment, a : Asset.Id) = Action(i, segment, a).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }
}
