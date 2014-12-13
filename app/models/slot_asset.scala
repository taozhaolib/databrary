package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import macros.async._
import dbrary._
import site._

final case class Excerpt(segment : Segment, classification : Classification.Value) {
  def apply(seg : Segment) : Boolean = segment @> seg
}

sealed trait AssetSlot extends Slot {
  def slotAsset : SlotAsset
  def excerpt : Option[Excerpt]
  def asset : Asset = slotAsset.asset
  final def assetId = asset.id
  override final def volume = asset.volume
  protected final def assetSegment = slotAsset.segment
  def format = asset.format
  def entire : Boolean
  def classification = excerpt.fold(asset.classification)(e => max(asset.classification, e.classification))

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  final override val permission : Permission.Value =
    volume.dataPermission(classification, consent).permission

  final override def auditDownload(implicit site : Site) : Future[Boolean] =
    Audit.download("slot_asset", 'container -> containerId, 'segment -> segment, 'asset -> assetId)

  override def fileName : Future[String] =
    for {
      vol <- volume.fileName
      slot <- super.fileName
    } yield {
      store.fileName(Seq(vol) ++ Maybe(slot).opt ++ asset.name.map(store.truncate(_)) : _*)
    }

  override def pageURL = controllers.routes.AssetSlotHtml.view(containerId, segment, assetId)

  override def json : JsonObject = JsonObject.flatten(
    Some('permission -> permission),
    if (segment.isFull) None else Some('segment -> segment),
    consentJson, // probably no-one uses this
    excerpt.map('excerpt -> _.classification)
  )

  def json(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(json.obj, options
    )
}

sealed trait FileAssetSlot extends AssetSlot with BackedAsset {
  def slotAsset : SlotFileAsset
  override def asset : FileAsset = slotAsset.asset
  def source = asset.source
  override def format = asset.format
}

sealed trait TimeseriesAssetSlot extends FileAssetSlot with TimeseriesData {
  def slotAsset : SlotTimeseriesAsset
  override def asset : TimeseriesAsset = slotAsset.asset
  override def source = asset.source
  override def format = super[TimeseriesData].format

  def section = segment.singleton.fold {
      /* We need to determine the portion of this asset and the slot which overlap, in asset-source space: */
      /* it must be within (and default to) this asset's own space */
      val l = asset.duration
      /* shifted forward if the slot starts later than the asset */
      val t0 = (for { s <- segment.lowerBound ; p <- assetSegment.lowerBound ; if s > p }
        yield (s - p)).getOrElse(Offset.ZERO)
      /* the lesser of the slot end and the asset end */
      val t1 = l + (for { s <- segment.upperBound ; p <- assetSegment.upperBound ; if s < p }
        yield (s - p)).getOrElse(Offset.ZERO)
      Segment(t0, t1)
    } { s =>
      Range.singleton[Offset](s - assetSegment.lowerBound.getOrElse(Offset.ZERO))
    }
}

/** An entire asset in its assigned position. */
sealed class SlotAsset protected (override val asset : Asset, final val segment : Segment, final val context : ContextSlot, final val excerpt : Option[Excerpt])
  extends AssetSlot {
  def slotAsset = this
  private[models] def sqlKey = asset.sqlKey
  def entire = true

  override def pageName = asset.pageName
  override def pageParent = Some(container)
    
  override def json : JsonObject = asset.json ++ super.json +
    ('container -> container.json)
}

sealed class SlotFileAsset private[models] (override val asset : FileAsset, segment : Segment, context : ContextSlot, excerpt : Option[Excerpt])
  extends SlotAsset(asset, segment, context, excerpt) with FileAssetSlot {
  override def slotAsset = this
}

final class SlotTimeseriesAsset private[models] (override val asset : TimeseriesAsset, segment : Segment, context : ContextSlot, excerpt : Option[Excerpt])
  extends SlotFileAsset(asset, segment, context, excerpt) with TimeseriesAssetSlot {
  override def slotAsset = this
}

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an SlotAsset within the context of a segment. */
sealed class AssetSegment private[models] (val slotAsset : SlotAsset, _segment : Segment, final val context : ContextSlot, final val excerpt : Option[Excerpt])
  extends AssetSlot {
  final val segment = slotAsset.segment * _segment
  private[models] def sqlKey = SQLTerms('asset -> assetId, 'segment -> segment)
  def entire = false

  override def pageName = _segment.toString
  override def pageParent = Some(slotAsset)
    
  override def json : JsonObject = super.json ++ JsonObject.flatten(
    Some('asset -> slotAsset.json),
    if (format === asset.format) None else Some[JsonField]('format -> format.id),
    excerpt.filter(_ => slotAsset.permission < permission).map(e => 'context -> e.segment)
  )
}

sealed class FileAssetSegment private[models] (override val slotAsset : SlotFileAsset, segment : Segment, context : ContextSlot, excerpt : Option[Excerpt])
  extends AssetSegment(slotAsset, segment, context, excerpt) with FileAssetSlot

final class TimeseriesAssetSegment private[models] (override val slotAsset : SlotTimeseriesAsset, segment : Segment, context : ContextSlot, excerpt : Option[Excerpt])
  extends FileAssetSegment(slotAsset, segment, context, excerpt) with TimeseriesAssetSlot


object SlotAsset extends Table[SlotAsset]("slot_asset") with TableSlot[SlotAsset] {
  private[models] def apply(asset : Asset, segment : Segment, context : ContextSlot, excerpt : Option[Excerpt]) : SlotAsset =
    asset match {
      case ts : TimeseriesAsset => new SlotTimeseriesAsset(ts, segment, context, excerpt)
      case f : FileAsset => new SlotFileAsset(f, segment, context, excerpt)
      case a => new SlotAsset(a, segment, context, excerpt)
    }

  private[models] val columns = Columns(
    SelectColumn[Segment]("segment")
  )

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    columns.join(
      ContextSlot.rowContainer(
        Container.columnsVolume(Volume.fixed(asset.volume)) on "slot_asset.container = container.id",
        "slot_asset.segment"),
      Excerpt.columns on_? "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment"
    ).map { case (segment, context, excerpt) =>
      SlotAsset(asset, segment, context, excerpt)
    }
    .SELECT("WHERE slot_asset.asset = ?")
    .apply(asset.id).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    columns.join(
      Asset.rowVolume(slot.volume) on "slot_asset.asset = asset.id",
      ContextSlot.rowContainer(slot.container, "slot_asset.container", "slot_asset.segment"),
      Excerpt.columns on_? "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment"
    ).map { case (segment, asset, context, excerpt) =>
      SlotAsset(asset, segment, context, excerpt)
    }
    .SELECT("WHERE slot_asset.segment && ? ORDER BY slot_asset.segment")
    .apply(slot.segment).list
}

object AssetSlot extends Table[AssetSlot]("slot_asset") with TableSlot[AssetSlot] {
  private def apply(slotAsset : SlotAsset, seg : Segment, context : ContextSlot, excerpt : Option[Excerpt]) : AssetSlot =
    slotAsset match {
      case ts : SlotTimeseriesAsset => new TimeseriesAssetSegment(ts, seg, context, excerpt)
      case f : SlotFileAsset => new FileAssetSegment(f, seg, context, excerpt)
      case a => new AssetSegment(a, seg, context, excerpt)
    }
  private[models] def apply(asset : Asset, segment : Segment, seg : Segment, context : ContextSlot, excerpt : Option[Excerpt]) : AssetSlot =
    if (seg === segment)
      SlotAsset(asset, segment, context, excerpt)
    else
      AssetSlot(SlotAsset(asset, segment, context.contextFor(segment), excerpt.filter(_(segment))), seg, context, excerpt)

  private def columnsSegment(seg : Segment) = SlotAsset.columns join Columns(
      SelectColumn[Segment]("asset_slot", "segment")
    )(FromTable("LATERAL (VALUES (slot_asset.segment * ?)) AS asset_slot (segment)")).cross
    .pushArgs(seg)

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume) which must also be the asset's volume. */
  def get(assetId : Asset.Id, containerId : Container.Id, seg : Segment)(implicit site : Site) : Future[Option[AssetSlot]] =
    columnsSegment(seg).join(
      Asset.columns on "slot_asset.asset = asset.id",
      ContextSlot.rowContainer(
        Container.columnsVolume(Volume.row) on "slot_asset.container = container.id AND asset.volume = volume.id",
        "asset_slot.segment"),
      Excerpt.columns on_? "slot_asset.asset = excerpt.asset AND asset_slot.segment <@ excerpt.segment"
    ).map { case ((segment, seg), asset, context, excerpt) =>
      AssetSlot(asset(context.volume), segment, seg, context, excerpt)
    }
    .SELECT("WHERE asset.id = ? AND container.id = ? AND", Volume.condition)
    .apply(assetId, containerId).singleOpt
}

object Excerpt extends Table[AssetSlot]("excerpt") with TableSlot[AssetSlot] {
  private[models] val columns = Columns(
      segment
    , SelectColumn[Classification.Value]("classification")
    ).map(Excerpt.apply _)

  private def rowVolume(volume : Volume) =
    columns.join(
      SlotAsset.columns on "excerpt.asset = slot_asset.asset",
      Asset.columns.map(_(volume)) on "slot_asset.asset = asset.id AND asset.sha1 IS NOT NULL",
      ContextSlot.rowContainer(
        Container.columns.map(_(volume)) on "slot_asset.container = container.id",
        "excerpt.segment")
    ).map { case (excerpt, segment, asset, context) =>
      AssetSlot(asset, segment, excerpt.segment, context, Some(excerpt)).asInstanceOf[FileAssetSlot]
    }

  /** Retrieve the list of all excerpts on a volume. */
  private[models] def getVolume(vol : Volume) : Future[Seq[FileAssetSlot]] =
    rowVolume(vol)
    .SELECT("WHERE container.volume = ? AND asset.volume = ?")
    .apply(vol.id, vol.id).list

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getVolumeThumb(vol : Volume) : Future[Option[FileAssetSlot]] =
    rowVolume(vol)
    .SELECT("JOIN format ON asset.format = format.id",
      "WHERE container.volume = ? AND asset.volume = ?",
        "AND GREATEST(excerpt.classification, asset.classification) >= read_classification(?::permission, slot_consent.consent)",
        "AND (asset.duration IS NOT NULL AND format.mimetype LIKE 'video/%' OR format.mimetype LIKE 'image/%')",
      "ORDER BY container.top DESC LIMIT 1")
    .apply(vol.id, vol.id, vol.permission).singleOpt

  /** Retrieve the list of all excerpts in a slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[AssetSlot]] =
    columns.join(
      SlotAsset.columns on "excerpt.asset = slot_asset.asset",
      Asset.columns.map(_(slot.volume)) on "slot_asset.asset = asset.id",
      ContextSlot.rowContainer(slot.container, "slot_asset.container", "excerpt.segment")
    ).map { case (excerpt, segment, asset, context) =>
      AssetSlot(asset, segment, excerpt.segment, context, Some(excerpt))
    }
    .SELECT("WHERE asset.volume = ? AND excerpt.segment && ?")
    .apply(slot.volumeId, slot.segment).list

  def set(asset : Asset, segment : Segment, classification : Option[Classification.Value]) : Future[Boolean] = {
    implicit val site = asset.site
    val key = SQLTerms('asset -> asset.id)
    classification.fold {
      Audit.remove("excerpt", key :+ SQLTerm.eq("segment", "&&", segment))
    } { classification =>
      Audit.changeOrAdd("excerpt", SQLTerms('classification -> classification), key :+ ('segment -> segment))
    }.execute.recover {
      case SQLDuplicateKeyException() => false
    }
  }
}

