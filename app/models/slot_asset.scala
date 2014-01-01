package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an ContainerAsset within the context of a Slot. */
sealed class SlotAsset protected (val asset : Asset, asset_segment : Segment, val slot : AbstractSlot, excerpt_segment : Option[Segment]) extends TableRow with BackedAsset with SiteObject with InVolume {
  def volume = asset.volume
  def assetId = asset.id
  def source = asset.source
  override def format = asset.format
  def position = asset_segment.lowerBound.map(_ - slot.segment.lowerBound.getOrElse(Offset.ZERO))
  require(excerpt_segment.fold(true)(slot.segment @> _))
  def excerpt = excerpt_segment.isDefined

  def classification = asset.classification match {
    case Classification.IDENTIFIED if excerpt => Classification.EXCERPT
    case c => c
  }

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  override lazy val getPermission : Permission.Value =
    Permission.data(asset.getPermission, slot.getConsent, classification).getPermission

  def in(s : Slot) =
    if (s.equals(slot))
      this
    else
      SlotAsset.make(asset, asset_segment, s, excerpt_segment.filter(slot.segment @> _))
  /** "Expand" this slot asset to a larger one with equivalent permissions.
    * This determines what segment should be shown to users when they request a smaller one.
    */
  def inContext : SlotAsset = {
    val c = in(slot.context)
    if (c.getPermission < getPermission)
      this
    else {
      val a = c.in(slot.container)
      if (a.getPermission < c.getPermission)
        c
      else
        a
    }
  }

  def containingSlot : Slot = slot match {
    case s : Slot => s
    case _ => slot.context
  }
  def pageName = asset.name
  def pageParent = slot match {
    case p : SitePage => Some(p)
    case _ => Some(slot.context)
  }
  def pageURL = controllers.routes.SlotAssetHtml.view(slot.containerId, slot.segment.lowerBound, slot.segment.upperBound, assetId)
  def pageActions = Seq(
      Action("view", pageURL, Permission.VIEW)
    ) ++ (if (slot.isFull) Seq(
      Action("edit", controllers.routes.AssetHtml.edit(volumeId, assetId), Permission.EDIT),
      Action("remove", controllers.routes.AssetHtml.remove(volumeId, assetId), Permission.CONTRIBUTE)
    ) else Nil)

  lazy val json : JsonObject = JsonObject(
    'asset -> (asset.json ++
      JsonObject.flatten(if (asset_segment.isFull) None else Some('segment -> asset_segment))),
    'permission -> getPermission
  ) ++ slot.json
}

final class SlotTimeseries private[models] (override val asset : Timeseries, asset_segment : Segment, slot : AbstractSlot, excerpt_segment : Option[Segment]) extends SlotAsset(asset, asset_segment, slot, excerpt_segment) with TimeseriesData {
  override def source = asset.source
  def segment = slot.segment.singleton.fold {
      /* We need to determine the portion of this asset and the slot which overlap, in asset-source space: */
      /* it must be within (and default to) this asset's own space */
      val l = asset.duration
      /* shifted forward if the slot starts later than the asset */
      val t0 = (for { s <- slot.segment.lowerBound ; p <- asset_segment.lowerBound ; if s > p }
        yield (s - p)).getOrElse(Offset.ZERO)
      /* the lesser of the slot end and the asset end */
      val t1 = l + (for { s <- slot.segment.upperBound ; p <- asset_segment.upperBound ; if s < p }
        yield (s - p)).getOrElse(Offset.ZERO)
      Segment(t0, t1)
    } { s =>
      Range.singleton[Offset](s - asset_segment.lowerBound.getOrElse(Offset.ZERO))
    }
  def entire = slot.segment @> asset_segment
}

object SlotAsset extends Table[SlotAsset]("slot_asset") {
  private[models] def make(asset : Asset, segment : Segment, slot : AbstractSlot, excerpt : Option[Segment]) = asset match {
    case ts : Timeseries => new SlotTimeseries(ts, segment, slot, excerpt)
    case _ => new SlotAsset(asset, segment, slot, excerpt)
  }
  private val columns = Columns(
      SelectColumn[Segment]("segment")
    , SelectColumn[Option[Segment]]("excerpt")
    )
    .join(Asset.columns, "slot_asset.asset = asset.id")
    .map { case ((segment, excerpt), asset) =>
      (slot : Slot) => make(asset(slot.volume), segment, slot, excerpt)
    }
  private def slotRow(slot : Slot) = columns
    .map(_(slot))
  private def volumeRow(volume : Volume) = columns
    .join(Slot.volumeRow(volume), "slot_asset.slot = slot.id AND asset.volume = container.volume")
    .map { case (asset, slot) => asset(slot) }
  private def row(full : Boolean)(implicit site : Site) = columns
    .join(if (full) Container.row else Slot.row, "slot_asset.slot = slot.id AND asset.volume = container.volume")
    .map { case (asset, slot) => asset(slot) }
  private def base(segment : Segment) = Columns(
      SelectColumn[Segment]("slot_asset", "segment")
    , SelectColumn[Option[Segment]]("slot_excerpt", "segment")
    )
    .from("""asset_slot JOIN slot AS slot_asset ON asset_slot.slot = slot_asset.id
      LEFT JOIN excerpt JOIN slot AS slot_excerpt ON excerpt.slot = slot_excerpt.id
        ON asset_slot.asset = excerpt.asset AND slot_asset.source = slot_excerpt.source AND ?::segment <@ slot_excerpt.segment""")
    .pushArgs(SQLArgs(segment))
    .join(Asset.columns, "asset_slot.asset = asset.id")
  private def abstractSlotRow(slot : AbstractSlot) = base(slot.segment)
    .map { case ((segment, excerpt), asset) =>
      make(asset(slot.volume), segment, slot, excerpt)
    }

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume) which must also be the asset's volume.
    * @param full only return full slots */
  def get(asset : Asset.Id, slot : Slot.Id, full : Boolean = false)(implicit site : Site) : Future[Option[SlotAsset]] =
    row(full)
      .SELECT("WHERE asset = ? AND slot = ? AND", Volume.condition)
      .apply(SQLArgs(asset, slot) ++ Volume.conditionArgs).singleOpt

  def get(asset : Asset.Id, slot : Slot.Id, segment : Segment)(implicit site : Site) : Future[Option[SlotAsset]] =
    if (segment.isFull) /* may be container or actual slot id */
      get(asset, slot, false)
    else /* must be container id */ base(segment)
      .join(Slot.abstractRow(segment), "slot_asset.source = container.id AND slot_asset.segment && ?::segment AND asset.volume = container.volume")
      .map { case (((segment, excerpt), asset), slot) =>
        make(asset(slot.volume), segment, slot, excerpt)
      }
      .SELECT("WHERE asset.id = ? AND container.id = ? AND", Volume.condition)
      .apply(SQLArgs(segment, asset, slot) ++ Volume.conditionArgs).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    slotRow(slot)
      .SELECT("WHERE asset.volume = ? AND slot_asset.slot = ?")
      .apply(slot.volumeId, slot.id).list

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlotAll(slot : AbstractSlot) : Future[Seq[SlotAsset]] =
    abstractSlotRow(slot)
      .SELECT("WHERE slot_asset.source = ? AND slot_asset.segment && ?::segment AND asset.volume = ?")
      .apply(slot.containerId, slot.segment, slot.volumeId).list

  /** Retrieve the list of all foreign assets (from a different volume) within the given slot. */
  private[models] def getSlotForeign(slot : AbstractSlot)(implicit site : Site) : Future[Seq[SlotAsset]] =
    base(slot.segment).join(Volume.row, "asset.volume = volume.id")
      .map { case (((segment, excerpt), asset), vol) =>
        make(asset(vol), segment, slot, excerpt)
      }
      .SELECT("WHERE slot_asset.source = ? AND slot_asset.segment && ?::segment AND asset.volume <> ? AND", Volume.condition)
      .apply(SQLArgs(slot.containerId, slot.segment, slot.volumeId) ++ Volume.conditionArgs).list

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    Slot.volumeRow(asset.volume)
      .map { slot => make(asset, slot.segment, slot, None /* XXX */) }
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
        .SELECT("WHERE excerpt IS NOT NULL AND asset.volume = ?")
        .apply(volume.id).list
      s <- volume.top
      t <- getSlot(s)
    } yield (l ++ t)

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getThumb(volume : Volume)(implicit site : Site) : Future[Option[SlotAsset]] =
    volumeRow(volume).SELECT("""
       JOIN format ON asset.format = format.id
      WHERE (excerpt IS NOT NULL OR container.top AND slot.segment = '(,)' OR slot.consent >= 'PRIVATE')
        AND (asset.duration IS NOT NULL OR format.mimetype LIKE 'image/%')
        AND data_permission(?::permission, context.consent, asset.classification, ?::permission, excerpt IS NOT NULL) >= 'DOWNLOAD'
        AND asset.volume = ?
        ORDER BY excerpt NULLS LAST, container.top DESC, slot.consent DESC NULLS LAST LIMIT 1""")
      .apply(volume.getPermission, site.access, volume.id).singleOpt

  /** Find an asset suitable for use as a slot thumbnail. */
  private[models] def getThumb(slot : AbstractSlot)(implicit site : Site) : Future[Option[SlotAsset]] =
    abstractSlotRow(slot)
      .SELECT("""JOIN format ON asset.format = format.id
      WHERE slot_asset.source = ? AND slot_asset.segment && ?::segment AND asset.volume = ?
        AND (asset.duration IS NOT NULL OR format.mimetype LIKE 'image/%') 
        AND data_permission(?::permission, ?::consent, asset.classification, ?::permission, slot_excerpt.segment IS NOT NULL) >= 'DOWNLOAD'
        ORDER BY excerpt NULLS LAST LIMIT 1""")
      .apply(slot.containerId, slot.segment, slot.volumeId, slot.getPermission, slot.getConsent, site.access).singleOpt
}
