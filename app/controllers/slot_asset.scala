package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import models._

private[controllers] sealed class SlotAssetController extends ObjectController[SlotAsset] {
  private[controllers] def action(i : models.Slot.Id, start : Option[Offset] = None, end : Option[Offset] = None, a : models.Asset.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(models.SlotAsset.get(a, i, Range[Offset](start, end))(_), p)

  private[controllers] def Action(i : models.Slot.Id, start : Option[Offset] = None, end : Option[Offset] = None, a : models.Asset.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(i, start, end, a, p)

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

  def download(s : models.Slot.Id, start : Option[Offset], end : Option[Offset], o : models.Asset.Id, inline : Boolean) = Action(s, start, end, o, Permission.DOWNLOAD).async { implicit request =>
    AssetController.assetResult(request.obj, if (inline) None else Some(request.obj.asset.name))
  }

  def frame(i : models.Slot.Id, start : Option[Offset], end : Option[Offset], o : models.Asset.Id, eo : Offset) = Action(i, start, end, o, Permission.DOWNLOAD).async { implicit request =>
    getFrame(Right(eo))
  }
  def head(i : models.Slot.Id, start : Option[Offset], end : Option[Offset], o : models.Asset.Id) =
    frame(i, start, end, o, Offset.ZERO)
  def thumb(i : models.Slot.Id, start : Option[Offset], end : Option[Offset], o : models.Asset.Id) = Action(i, start, end, o, Permission.DOWNLOAD).async { implicit request =>
    getFrame(Left(0.25f))
  }
}

object SlotAssetController extends SlotAssetController

object SlotAssetHtml extends SlotAssetController {
  def view(i : models.Slot.Id, start : Option[Offset], end : Option[Offset], a : models.Asset.Id) = Action(i, start, end, a) { implicit request =>
    Ok(views.html.asset.view(request.obj))
  }
}
