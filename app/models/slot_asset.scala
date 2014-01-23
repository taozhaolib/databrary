package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import dbrary._
import site._

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an ContainerAsset within the context of a Slot. */
sealed class SlotAsset protected (val asset : Asset, asset_segment : Segment, val slot : Slot, excerpt_segment : Option[Segment]) extends Slot with TableRow with BackedAsset with InVolume with SiteObject {
  val segment = slot.segment * asset_segment
  def context = slot.context
  def volume = asset.volume
  def assetId = asset.id
  def source = asset.source
  override def format = asset.format

  override def position : Option[Offset] = asset_segment.lowerBound.map(_ - slot.position)
  require(excerpt_segment.fold(true)(_ @> segment))
  def excerpt = excerpt_segment.isDefined

  def classification = asset.classification match {
    case Classification.IDENTIFIED if excerpt => Classification.EXCERPT
    case c => c
  }

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  override lazy val permission : Permission.Value =
    Permission.data(asset.permission, consent, classification).permission

  def in(s : Slot) =
    if (slot === s)
      this
    else
      SlotAsset.make(asset, asset_segment, s, excerpt_segment.filter(s.segment @> _))
  /** "Expand" this slot asset to a larger one with equivalent permissions.
    * This determines what segment should be shown to users when they request a smaller one.
    */
  def inContext : SlotAsset = {
    val c = in(slot.context)
    if (c.permission < permission)
      this
    else {
      val a = c.in(slot.container)
      if (a.permission < c.permission)
        c
      else
        a
    }
  }

  def pageName = asset.pageName
  def pageParent = Some(slot)
  def pageURL = controllers.routes.SlotAssetHtml.view(containerId, slot.segment, assetId)
  def pageActions = Seq(
      Action("view", pageURL, Permission.VIEW)
    ) ++ (if (slot.isFull) Seq(
      Action("edit", controllers.routes.AssetHtml.edit(assetId), Permission.EDIT),
      Action("remove", controllers.routes.AssetHtml.remove(assetId), Permission.CONTRIBUTE)
    ) else Nil)

  lazy val json : JsonObject = JsonObject.flatten(
    Some('permission -> permission),
    if (format === asset.format) None else Some('format -> format.json),
    Some('asset -> (asset.json ++
      JsonObject.flatten(if (asset_segment.isFull) None else Some('segment -> asset_segment))))
  ) ++ slot.json

  def json(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(json.obj, options
    )
}

final class SlotTimeseries private[models] (override val asset : Timeseries, asset_segment : Segment, val slot : Slot, excerpt_segment : Option[Segment]) extends SlotAsset(asset, asset_segment, slot, excerpt_segment) with TimeseriesData {
  override def source = asset.source
  def section = segment.singleton.fold {
      /* We need to determine the portion of this asset and the slot which overlap, in asset-source space: */
      /* it must be within (and default to) this asset's own space */
      val l = asset.duration
      /* shifted forward if the slot starts later than the asset */
      val t0 = (for { s <- segment.lowerBound ; p <- asset_segment.lowerBound ; if s > p }
        yield (s - p)).getOrElse(Offset.ZERO)
      /* the lesser of the slot end and the asset end */
      val t1 = l + (for { s <- segment.upperBound ; p <- asset_segment.upperBound ; if s < p }
        yield (s - p)).getOrElse(Offset.ZERO)
      Segment(t0, t1)
    } { s =>
      Range.singleton[Offset](s - asset_segment.lowerBound.getOrElse(Offset.ZERO))
    }
  def entire = segment @> asset_segment
}

object SlotAsset extends Table[SlotAsset]("slot_asset") {
  private[models] def make(asset : Asset, asset_segment : Segment, slot : Slot, excerpt : Option[Segment]) = asset match {
    case ts : Timeseries => new SlotTimeseries(ts, asset_segment, slot, excerpt)
    case _ => new SlotAsset(asset, asset_segment, slot, excerpt)
  }

  private sealed abstract class SlotAssetTable(table : String) extends SlotTable(table) {
    final def containerRow(container : Selector[Container]) =
      rowContainer(container)
      .join(Asset.columns, "slot_asset.asset = asset.id")
      .map { case (slot, asset) =>
	make(asset(slot.volume), slot.segment, slot, slot.segment)
      }
    def isExcerpt = false

    final def getThumb(volume : Volume)(implicit site : Site) : Future[Option[SlotAsset]] =
      containerRow(Container.volumeRow(volume))
      .SELECT("""JOIN format ON asset.format = format.id
	WHERE container.volume = ? AND asset.volume = container.volume
	  AND (asset.duration IS NOT NULL OR format.mimetype LIKE 'image/%')
	  AND data_permission(?::permission, consent, asset.classification, ?::permission, """ + isExcerpt +++ """) >= 'DOWNLOAD'
	ORDER BY container.top DESC, consent DESC NULLS LAST LIMIT 1""")
      .apply(volume.id, volume.permission, site.access).singleOpt
  }

  private object SlotAssetSlot extends SlotAssetTable("slot_asset") {
  }

  private object Excerpt extends SlotAssetTable("excerpt") {
    override def isExcerpt = false
    protected override val columnsContext =
      super.columnsContext from
	"(SELECT container, excerpt.segment, asset FROM slot_asset JOIN " + table + " USING (asset)) AS " + table
  }

  private def columnsSlotAsset[A](slot : Selector[Slot], asset : Selector[A]) : Selector[(((Slot, Segment), Option[Segment]), A)] =
    slot
    .join(SlotAssetSlot.columns, "slot.container = slot_asset.container AND slot.segment && slot_asset.segment")
    .leftJoin(Excerpt.columns, "excerpt.segment @> slot.segment")
    .join(asset, "slot_asset.asset = asset.id")
  private def rowSlot(slot : Selector[Slot]) : Selector[SlotAsset] =
    columnsSlotAsset(slot, Asset.columns)
    .map { case (((slot, segment), excerpt), asset) =>
      make(asset(slot.volume), segment, slot, excerpt)
    }

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume) which must also be the asset's volume.
    * @param full only return full slots */
  def get(assetId : Asset.Id, containerId : Container.Id, segment : Segment)(implicit site : Site) : Future[Option[SlotAsset]] =
    rowSlot(Slot.row(containerId, segment))
    .SELECT("WHERE asset.id = ? AND asset.volume = container.volume AND", Volume.condition)
    .apply(assetId).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    rowSlot(Slot.fixed(slot))
    .SELECT("WHERE asset.volume = ?")
    .apply(slot.volumeId).list

  /** Retrieve the list of all foreign assets (from a different volume) within the given slot. */
  private[models] def getSlotForeign(slot : Slot)(implicit site : Site) : Future[Seq[SlotAsset]] =
    columnsSlotAsset(Slot.fixed(slot), Asset.row)
    .map { case (((slot, segment), excerpt), asset) =>
      make(asset, segment, slot, excerpt)
    }
    .SELECT("WHERE asset.volume <> ? AND", Volume.condition)
    .apply(slot.volumeId).list

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    SlotAssetSlot.rowContainer(Container.volumeRow(asset.volume))
    // .leftJoin(Excerpt.columns, "excerpt.segment @> slot_asset.segment") // this is unlikely
    .map { slot =>
      make(asset, slot.segment, slot, None)
    }
    .SELECT("WHERE slot_asset.asset = ? AND container.volume = ?")
    .apply(asset.id, asset.volumeId).singleOpt

  /** Retrieve the list of all assets assigned the given record. */
  private[models] def getRecord(record : Record) : Future[Seq[SlotAsset]] =
    rowSlot(SlotRecord.row(record))
    .SELECT("WHERE slot_record.record = ? AND container.volume = ? AND asset.volume = container.volume")
    .apply(record.id, record.volumeId).list

  private[models] def getExcerpt(volume : Volume) : Future[Seq[SlotAsset]] =
    Excerpt.containerRow(Container.volumeRow(volume))
    .SELECT("WHERE container.volume = ? AND asset.volume = container.volume")
    .apply(volume.id).list

  /** Retrieve the list of all top-level assets. */
  private[models] def getToplevel(volume : Volume) : Future[Seq[SlotAsset]] =
    for {
      e <- getExcerpt(volume)
      s <- volume.top
      t <- getSlot(s)
    } yield (e ++ t)

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getThumb(volume : Volume)(implicit site : Site) : Future[Option[SlotAsset]] =
    Excerpt.getThumb(volume)
      flatMap Async.orElse(_,
    SlotAssetSlot.getThumb(volume))

  /** Find an asset suitable for use as a slot thumbnail. */
  private[models] def getThumb(slot : Slot)(implicit site : Site) : Future[Option[SlotAsset]] =
    abstractSlotRow(slot)
      .SELECT("""JOIN format ON asset.format = format.id
      WHERE slot_asset.source = ? AND slot_asset.segment && ?::segment AND asset.volume = ?
        AND (asset.duration IS NOT NULL OR format.mimetype LIKE 'image/%') 
        AND data_permission(?::permission, ?::consent, asset.classification, ?::permission, slot_excerpt.segment IS NOT NULL) >= 'DOWNLOAD'
        ORDER BY excerpt NULLS LAST LIMIT 1""")
      .apply(slot.containerId, slot.segment, slot.volumeId, slot.permission, slot.getConsent, site.access).singleOpt
}
