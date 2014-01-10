package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import models._

private[controllers] sealed class SlotAssetController extends ObjectController[SlotAsset] {
  private[controllers] def action(i : models.Slot.Id, segment : Segment, a : models.Asset.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(models.SlotAsset.get(a, i, segment)(_), p)

  private[controllers] def Action(i : models.Slot.Id, segment : Segment, a : models.Asset.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(i, segment, a, p)

  private[controllers] def getFrame(offset : Either[Float,Offset])(implicit request : Request[_]) =
    request.obj match {
      case ts : SlotTimeseries =>
        /* round down to a 10-second boundry, which is our i-frame interval. */
        val off = offset.fold[Offset](f => Offset(10000L*(f*ts.duration.millis/10000).toLong), o => o)
        if (off < Offset.ZERO || off > ts.duration)
          ANotFound
        else
          AssetController.assetResult(ts.sample(off))
      case _ =>
        if (!offset.fold(_ => true, _ == 0))
          ANotFound
        else
          AssetController.assetResult(request.obj)
    }

  def download(s : models.Slot.Id, segment : Segment, o : models.Asset.Id, inline : Boolean) = Action(s, segment, o, Permission.DOWNLOAD).async { implicit request =>
    AssetController.assetResult(request.obj, if (inline) None else Some(request.obj.asset.name.getOrElse("file")))
  }

  def frame(i : models.Container.Id, o : models.Asset.Id, eo : Offset) = head(i, Range.singleton(eo), o)
  def head(i : models.Slot.Id, segment : Segment, o : models.Asset.Id) = Action(i, segment, o, Permission.DOWNLOAD).async { implicit request =>
    getFrame(Right(Offset.ZERO))
  }
  def thumb(i : models.Slot.Id, segment : Segment, o : models.Asset.Id) = Action(i, segment, o, Permission.DOWNLOAD).async { implicit request =>
    getFrame(Left(0.25f))
  }
}

object SlotAssetController extends SlotAssetController

object SlotAssetHtml extends SlotAssetController {
  def view(i : models.Slot.Id, segment : Segment, a : models.Asset.Id) = Action(i, segment, a).async { implicit request =>
    for {
      comments <- request.obj.slot.comments
    } yield (Ok(views.html.asset.view(request.obj, comments)))
  }
}

object SlotAssetApi extends SlotAssetController {
  def get(i : Container.Id, segment : Segment, a : Asset.Id) = Action(i, segment, a).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }
}
