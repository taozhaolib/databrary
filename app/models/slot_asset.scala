package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an ContainerAsset within the context of a Slot. */
sealed class SlotAsset protected (val asset : Asset, asset_segment : Range[Offset], val slot : Slot, val excerpt : Boolean) extends TableRow with SiteObject with BackedAsset with InVolume {
  def slotId = slot.id
  def volume = asset.volume
  def assetId = asset.id
  def source = asset.source
  override def format = asset.format
  def etag = asset.etag
  def position = asset_segment.lowerBound.map(_ - slot.segment.lowerBound.getOrElse(0))

  def classification = asset.classification match {
    case Classification.IDENTIFIED if excerpt => Classification.EXCERPT
    case c => c
  }

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  override def getPermission : Permission.Value =
    Permission.data(asset.getPermission, slot.getConsent, classification).getPermission

  def in(s : Slot) =
    SlotAsset.make(asset, asset_segment, s, excerpt && slot.segment @> s.segment /* not quite right but should be fine for this use */)

  def pageName = asset.name
  def pageParent = if (slot.container.top) slot.pageParent else Some(slot)
  def pageURL = controllers.routes.SlotAsset.view(volume.id, slotId, assetId)
  def pageActions = Seq(
      Action("view", controllers.routes.SlotAsset.view(volumeId, slotId, assetId), Permission.VIEW)
    ) ++ (if (slot.isFull) Seq(
      Action("edit", controllers.routes.Asset.edit(volumeId, assetId), Permission.EDIT),
      Action("remove", controllers.routes.Asset.remove(volumeId, assetId), Permission.CONTRIBUTE)
    ) else Nil)
}

final class SlotTimeseries private[models] (override val asset : Timeseries, asset_segment : Range[Offset], slot : Slot, excerpt : Boolean) extends SlotAsset(asset, asset_segment, slot, excerpt) with TimeseriesData {
  override def source = asset.source
  def segment = slot.segment.singleton.fold {
      /* We need to determine the portion of this asset and the slot which overlap, in asset-source space: */
      /* it must be within (and default to) this asset's own space */
      val l = asset.duration
      /* shifted forward if the slot starts later than the asset */
      val t0 = (for { s <- slot.segment.lowerBound ; p <- asset_segment.lowerBound ; if s > p }
        yield (s - p)).getOrElse(0 : Offset)
      /* the lesser of the slot end and the asset end */
      val t1 = l + (for { s <- slot.segment.upperBound ; p <- asset_segment.upperBound ; if s < p }
        yield (s - p)).getOrElse(0 : Offset)
      Range[Offset](t0, t1)
    } { s =>
      Range.singleton[Offset](s - asset_segment.lowerBound.getOrElse(0 : Offset))
    }
  def entire = slot.segment @> asset_segment
  override def etag = if (entire) super.etag else "slot:" + slot.id + ":" + super.etag
  def sample(offset : Offset) = new TimeseriesSample(this, offset)
}

object SlotAsset extends Table[SlotAsset]("slot_asset") {
  private def make(asset : Asset, segment : Range[Offset], slot : Slot, excerpt : Boolean) = asset match {
    case ts : Timeseries => new SlotTimeseries(ts, segment, slot, excerpt)
    case _ => new SlotAsset(asset, segment, slot, excerpt)
  }
  private val columns = Columns(
      SelectColumn[Range[Offset]]("segment")
    , SelectColumn[Boolean]("excerpt")
    )
    .join(Asset.columns, "slot_asset.asset = asset.id")
    .map { case ((segment, excerpt), asset) =>
      (slot : Slot) => make(asset(slot.volume), segment, slot, excerpt)
    }
  private def slotRow(slot : Slot) = columns
    .map(_(slot))
  private def volumeRow(volume : Volume) = columns
    .join(Slot.volumeRow(volume, false), "slot_asset.slot = slot.id AND asset.volume = container.volume")
    .map { case (asset, slot) => asset(slot) }
  private def row(full : Boolean)(implicit site : Site) = columns
    .join(if (full) Slot.Full.row else Slot.row, "slot_asset.slot = slot.id AND asset.volume = container.volume")
    .map { case (asset, slot) => asset(slot) }

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume) which must also be the asset's volume.
    * @param full only return full slots */
  def get(asset : Asset.Id, slot : Slot.Id, full : Boolean = false)(implicit site : Site) : Future[Option[SlotAsset]] =
    row(full)
      .SELECT("WHERE asset = ? AND slot = ? AND", Volume.condition)
      .apply(SQLArgs(asset, slot) ++ Volume.conditionArgs).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    slotRow(slot)
      .SELECT("WHERE asset.volume = ? AND slot_asset.slot = ?")
      .apply(slot.volumeId, slot.id).list

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    Slot.volumeRow(asset.volume, false)
      .map { slot => make(asset, slot.segment, slot, false /* XXX */) }
      .SELECT("JOIN asset_slot ON slot.id = asset_slot.slot WHERE asset_slot.asset = ? AND container.volume = ?")
      .apply(asset.id, asset.volumeId).singleOpt

  /** Retrieve the list of all assets assigned the given record. */
  private[models] def getRecord(record : Record) : Future[Seq[SlotAsset]] =
    volumeRow(record.volume)
      .SELECT("JOIN slot_record ON slot.id = slot_record.slot WHERE asset.volume = ? AND container.volume = ? AND slot_record.record = ?")
      .apply(record.volumeId, record.volumeId, record.id).list

  /** Retrieve the list of all top-level assets. */
  private[models] def getToplevel(volume : Volume) : Future[Seq[SlotAsset]] =
    for {
      l <- volumeRow(volume)
        .SELECT("WHERE excerpt AND asset.volume = ?")
        .apply(volume.id).list
      s <- volume.topSlot
      t <- getSlot(s)
    } yield (l ++ t)

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getThumb(volume : Volume)(implicit site : Site) : Future[Option[SlotAsset]] =
    volumeRow(volume).SELECT("""
       JOIN format ON asset.format = format.id
      WHERE (excerpt OR container.top AND slot.segment = '(,)' OR slot.consent >= 'PRIVATE')
        AND (asset.duration IS NOT NULL OR format.mimetype LIKE 'image/%')
        AND data_permission(?::permission, context.consent, asset.classification, ?::permission, excerpt) >= 'DOWNLOAD'
        AND asset.volume = ?
        ORDER BY excerpt DESC, container.top DESC, slot.consent DESC NULLS LAST LIMIT 1""")
      .apply(volume.getPermission, site.access, volume.id).singleOpt

  /** Find an asset suitable for use as a slot thumbnail. */
  private[models] def getThumb(slot : Slot)(implicit site : Site) : Future[Option[SlotAsset]] =
    slotRow(slot).SELECT("""
       JOIN format ON asset.format = format.id
      WHERE asset.volume = ? AND slot_asset.slot = ?
        AND (asset.duration IS NOT NULL OR format.mimetype LIKE 'image/%') 
        AND data_permission(?::permission, ?::consent, asset.classification, ?::permission, excerpt) >= 'DOWNLOAD'
        ORDER BY excerpt DESC LIMIT 1""")
      .apply(slot.volumeId, slot.id, slot.getPermission, slot.getConsent, site.access).singleOpt
}
