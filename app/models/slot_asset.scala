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
  private[models] def sqlKey = asset.sqlKey
  final val segment = slot.segment * asset_segment
  final def context = slot.context
  final override def volume = asset.volume
  final def assetId = asset.id
  def source = asset.source
  override def format = asset.format

  def entire = slot.segment @> asset_segment
  /** Segment occupied by asset wrt slot position. */
  final def relativeSegment = segment.map(_ - slot.position)
  require(excerpt_segment.forall(_ @> segment))
  final def excerpt = excerpt_segment.isDefined

  final def classification = asset.classification match {
    case Classification.IDENTIFIED if excerpt => Classification.EXCERPT
    case c => c
  }

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  final override val permission : Permission.Value =
    Permission.data(asset.permission, consent, classification, top = container.top).permission

  final private def in(s : Slot) =
    if (slot === s)
      this
    else
      SlotAsset.make(asset, asset_segment, s, excerpt_segment.filter(s.segment @> _))
  /** "Expand" this slot asset to a larger one with equivalent permissions.
    * This determines what segment should be shown to users when they request a smaller one.
    */
  final def inContext : SlotAsset = {
    val c = in(slot.context)
    if (c.permission < permission)
      this
    else {
      val a = c.in(container)
      if (a.permission < c.permission)
        c
      else
        a
    }
  }
  final def inContainer : SlotAsset = in(container)

  final def auditDownload(implicit site : Site) : Future[Boolean] =
    Audit.action(Audit.Action.open, "slot_asset", SQLTerms('container -> containerId, 'segment -> segment, 'asset -> assetId)).execute

  override def pageName = asset.pageName
  override def pageParent = Some(slot)
  override def pageURL = controllers.routes.SlotAssetHtml.view(containerId, slot.segment, assetId)

  def fileName : Future[String] =
    idents.map { i =>
      SlotAsset.fileNamePad.replaceAllIn(
	(volume.alias.getOrElse(volume.name).take(16) +: (i ++ asset.name))
	.mkString("-"),
	"_")
    }

  override lazy val json : JsonObject = JsonObject.flatten(
    Some('permission -> permission),
    if (format === asset.format) None else Some('format -> format.json),
    if (excerpt) Some('excerpt -> excerpt) else None,
    Some('asset -> (asset.json ++
      JsonObject.flatten(if (asset_segment.isFull) None else Some('segment -> asset_segment))))
  ) ++ slotJson

  def json(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(json.obj, options
    )
}

final class SlotTimeseries private[models] (override val asset : Timeseries, asset_segment : Segment, slot : Slot, excerpt_segment : Option[Segment]) extends SlotAsset(asset, asset_segment, slot, excerpt_segment) with TimeseriesData {
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
}

object SlotAsset extends Table[SlotAsset]("slot_asset") {
  private[models] def make(asset : Asset, asset_segment : Segment, slot : Slot, excerpt : Option[Segment]) = asset match {
    case ts : Timeseries => new SlotTimeseries(ts, asset_segment, slot, excerpt)
    case _ => new SlotAsset(asset, asset_segment, slot, excerpt)
  }

  private sealed abstract class SlotAssetTable(table : String) extends SlotTable(table) {
    protected def isExcerpt : Boolean
    protected def segmentColumn : String = "segment"
    final def assetRow(slot : Selector[Slot], asset : Selector[Volume => Asset] = Asset.columns) : Selector[SlotAsset] =
      (slot ~ SelectColumn[Segment](segmentColumn))
      .join(asset, table + ".asset = asset.id")
      .map { case ((slot, segment), asset) =>
	SlotAsset.make(asset(slot.volume), segment, slot, if (isExcerpt) Some(slot.segment) else None)
      }
    final def assetRowVolume(volume : Volume, asset : Selector[Volume => Asset] = Asset.columns) =
      assetRow(rowVolume(Volume.fixed(volume)), asset)

    final def getThumb(volume : Volume) : Future[Option[SlotAsset]] =
      assetRowVolume(volume)
      .SELECT("JOIN format ON asset.format = format.id",
	"WHERE container.volume = ? AND asset.volume = container.volume",
	  "AND (asset.duration IS NOT NULL OR format.mimetype LIKE 'image/%')",
	  "AND data_permission(?::permission,", table + "_consent.consent, asset.classification, ?::permission,", isExcerpt.toString, ", container.top) >= 'DOWNLOAD'",
	"ORDER BY container.top DESC, asset.format DESC, duration(" + table + ".segment) NULLS LAST,", table + "_consent.consent DESC NULLS LAST LIMIT 1")
      .apply(volume.id, volume.permission, volume.site.access.group).singleOpt
    final def getThumb(slot : Slot) : Future[Option[SlotAsset]] =
      assetRow(rowContainer(slot.container))
      .SELECT("JOIN format ON asset.format = format.id",
	"WHERE", table + ".segment <@ ? AND asset.volume = ?",
	  "AND (asset.duration IS NOT NULL OR format.mimetype LIKE 'image/%')",
	  "AND asset.classification >= ?",
	"LIMIT 1")
      .apply(slot.segment, slot.volumeId, Classification.download(slot.permission, slot.consent, isExcerpt)(slot.site)).singleOpt
  }

  private object SlotAssetSlot extends SlotAssetTable("slot_asset") {
    /* we assume entire assets can never be excerpts */
    protected def isExcerpt = false
  }

  private object Excerpt extends SlotAssetTable("excerpt") {
    protected def isExcerpt = true
    override protected def segmentColumn : String = "asset_segment"
    protected override def columnsContext =
      super.columnsContext from
	"(SELECT container, excerpt.segment, asset, slot_asset.segment AS asset_segment FROM slot_asset JOIN " + table + " USING (asset)) AS " + table
  }

  private def row(slot : Selector[Slot], asset : Selector[Volume => Asset] = Asset.columns, slot_table : String = "slot") : Selector[SlotAsset] =
    slot
    .join(SlotAssetSlot.columns, slot_table + ".container = slot_asset.container AND " + slot_table + ".segment && slot_asset.segment")
    .leftJoin(Excerpt.columns, slot_table + ".segment <@ excerpt.segment AND slot_asset.asset = excerpt.asset")
    .join(asset, "slot_asset.asset = asset.id")
    .map { case (((slot, segment), excerpt), asset) =>
      make(asset(slot.volume), segment, slot, excerpt)
    }

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume) which must also be the asset's volume.
    * @param full only return full slots */
  def get(assetId : Asset.Id, containerId : Container.Id, segment : Segment)(implicit site : Site) : Future[Option[SlotAsset]] =
    row(Slot.row(containerId, segment))
    .SELECT("WHERE asset.id = ? AND asset.volume = container.volume AND", Volume.condition)
    .apply(assetId).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    row(Slot.fixed(slot))
    .SELECT("WHERE asset.volume = ?")
    .apply(slot.volumeId).list

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    SlotAssetSlot.assetRowVolume(asset.volume, Asset.fixed(asset).map(const _))
    .SELECT("WHERE slot_asset.asset = ? AND container.volume = ?")
    .apply(asset.id, asset.volumeId).singleOpt

  private[this] def excerpts(volume : Volume) : Future[Seq[SlotAsset]] =
    Excerpt.assetRowVolume(volume)
    .SELECT("WHERE asset.volume = container.volume AND data_permission(?::permission, excerpt_consent.consent, asset.classification, ?::permission, true, container.top) >= 'DOWNLOAD'")
    .apply(volume.permission, volume.site.access.group).list

  private[this] def slotExcerpts(volume : Volume) : Future[Seq[SlotAsset]] =
    SlotAssetSlot.assetRowVolume(volume)
    .SELECT("WHERE asset.volume = container.volume AND asset.classification = 'EXCERPT' AND data_permission(?::permission, slot_asset_consent.consent, asset.classification, ?::permission, false, container.top) >= 'DOWNLOAD'")
    .apply(volume.permission, volume.site.access.group).list

  /** Retrieve the list of all excerpts. */
  private[models] def getExcerpts(volume : Volume) : Future[Seq[SlotAsset]] =
    for {
      e <- excerpts(volume)
      s <- slotExcerpts(volume)
    } yield (e ++ s)

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getThumb(volume : Volume) : Future[Option[SlotAsset]] =
    Excerpt.getThumb(volume).flatMap(Async.orElse[SlotAsset](_,
      SlotAssetSlot.getThumb(volume)))

  /** Find an asset suitable for use as a slot thumbnail. */
  private[models] def getThumb(slot : Slot) : Future[Option[SlotAsset]] =
    Excerpt.getThumb(slot).flatMap(Async.orElse[SlotAsset](_,
      SlotAssetSlot.getThumb(slot)))

  private final val fileNamePad = "[\0-,/?\\\\]+".r
}
