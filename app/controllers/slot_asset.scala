package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import models._

object SlotAsset extends ObjectController[SlotAsset] {
  private[controllers] def action(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW, full : Boolean = false) =
    RequestObject.check(v, models.SlotAsset.get(a, i, full)(_), p)

  private[controllers] def Action(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW, full : Boolean = false) =
    SiteAction ~> action(v, i, a, p, full)

  def view(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id) = Action(v, i, a) { implicit request =>
    Ok(views.html.asset.view(request.obj))
  }

  def download(v : models.Volume.Id, s : models.Slot.Id, o : models.Asset.Id, inline : Boolean) = Action(v, s, o, Permission.DOWNLOAD).async { implicit request =>
    Asset.assetResult(request.obj, if (inline) None else Some(request.obj.asset.name))
  }

  def downloadOlder(v : models.Volume.Id, s : models.Slot.Id, a : models.Asset.Id, o : models.Asset.Id, inline : Boolean) = Action(v, s, a, Permission.DOWNLOAD).async { implicit request =>
    models.Asset.getOlder(request.obj.asset, o).flatMap(_.fold(ANotFound) { a =>
      Asset.assetResult(a, if (inline) None else Some(request.obj.asset.name))
    })
  }

  private[controllers] def getFrame(offset : Either[Float,Offset])(implicit request : Request[_]) =
    request.obj match {
      case ts : SlotTimeseries =>
        val off = offset.fold(f => Offset(10*(f*ts.duration.seconds/10).floor), identity)
        if (off < 0 || off > ts.duration)
          ANotFound
        else
          Asset.assetResult(ts.sample(off))
      case _ =>
        if (!offset.fold(_ => true, _ == 0))
          ANotFound
        else
          Asset.assetResult(request.obj)
    }

  def frame(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, eo : Offset) = Action(v, i, o, Permission.DOWNLOAD).async { implicit request =>
    getFrame(Right(eo))
  }
  def head(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id) =
    frame(v, i, o, 0)
  def thumb(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id) = Action(v, i, o, Permission.DOWNLOAD).async { implicit request =>
    getFrame(Left(0.25f))
  }
}
