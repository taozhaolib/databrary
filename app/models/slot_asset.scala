package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import macros.async._
import dbrary._
import site._

sealed trait AbstractAssetSlot extends AbstractSlot {
  def slotAsset : Asset
  def asset : Asset = slotAsset.asset
  final def assetId = asset.id
  override final def volume = asset.volume
  protected def assetSegment = slotAsset.segment
  final def entire = segment === assetSegment
  def format = asset.format
  def classification = asset.classification
}

sealed trait AbstractFileAssetSlot extends AbstractAssetSlot with BackedAsset {
  def slotAsset : SlotFileAsset
  def asset : FileAsset = slotAsset.asset
  def source = asset.source
}

sealed trait AbstractTimeseriesAssetSlot extends AbstractFileAssetSlot with TimeseriesData {
  def slotAsset : TimeseriesFileAsset
  def asset : TimeseriesAsset = slotAsset.asset
  def source = asset.source

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
sealed class SlotAsset protected (override val asset : Asset, val container : Container, val segment : Segment)
  extends AbstractSlot(container, segment) with AbstractAssetSlot {
  def slotAsset = this

  def json : JsonObject =
    asset.json ++ slotJson 
}

sealed class SlotFileAsset protected (override val asset : FileAsset, container : Container, segment : Segment)
  extends SlotAsset(asset, container, segment) with FileAssetSlot

sealed class SlotTimeseriesAsset protected (override val asset : TimeseriesAsset, container : Container, segment : Segment)
  extends SlotFileAsset(asset, container, segment) with TimeseriesAssetSlot

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an SlotAsset within the context of a segment. */
sealed class AssetSlot protected (val slotAsset : SlotAsset, segment : Segment, consent : Consent.Value, val excerpt : Option[Classification.Value])
  extends Slot(slotAsset.container, slotAsset.segment * segment, consent) with AbstractAssetSlot {
  final def classification = excerpt.fold(super.classification)(max(super.classification, _))

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  final override val permission : Permission.Value =
    slot.dataPermission(classification).permission

  final override def auditDownload(implicit site : Site) : Future[Boolean] =
    Audit.download("slot_asset", 'container -> containerId, 'segment -> segment, 'asset -> assetId)

  override def fileName : Future[String] =
    for {
      vol <- volume.fileName
      slot <- super.fileName
    } yield {
      store.fileName(Seq(vol) ++ Maybe(slot).opt ++ asset.name.map(store.truncate(_)) : _*)
    }

  override def pageName = asset.pageName
  override def pageParent = Some(container)
  override def pageURL = controllers.routes.AssetSlotHtml.view(containerId, segment, assetId)
    
  override def json : JsonObject = JsonObject.flatten(
    Some('permission -> permission),
    excerpt.map('excerpt -> _),
    Some('asset -> slotAsset.json),
    if (format === asset.format) None else Some('format -> format.id)
  ) ++ (slotJson - "container")

  def json(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(json.obj, options
    )
}

sealed class FileAssetSlot private[models] (override val slotAsset : SlotFileAsset, segment : Segment, consent : Consent.Value, excerpt : Option[Classification.Value])
  extends AssetSlot(slotAsset, segment, consent, excerpt) with AbstractFileAssetSlot

final class TimeseriesAssetSlot private[models] (override val slotAsset : SlotTimeseriesAsset, segment : Segment, consent : Consent.Value, excerpt : Option[Classification.Value])
  extends FileAssetSlot(slotAsset, segment, consent, excerpt) with AbstractTimeseriesAssetSlot


object SlotAsset extends Table[SlotAsset]("slot_asset") with TableSlot[SlotAsset] {
  private[models] def make(asset : Asset, container : Container, segment : Segment) : SlotAsset =
    asset match {
      case ts : TimeseriesAsset => new SlotTimeseriesAsset(ts, container, segment)
      case f : FileAsset => new SlotFileAsset(f, container, segment)
      case a => new SlotAsset(a, container, segment)
    }

  private[models] val columns = Columns(
    SelectColumn[Segment]("segment")
  )

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(assetId : Asset.Id) : Future[Option[SlotAsset]] =
    columns
    .join(Container.columns, "slot_asset.container = container.id")
    .join(Asset.columns, "slot_asset.asset = asset.id")
    .join(Volume.row, "container.volume = volume.id AND asset.volume = volume.id")
    .map { case (((segment, container), asset), volume) => make(asset(volume), container(volume), segment) }
    .SELECT("WHERE asset.id = ?")
    .apply(assetId).singleOpt

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    columns
    .join(Container.columnsVolume(asset.volume))
    .map { (segment, container) => make(asset, container, segment) }
    .SELECT("WHERE slot_asset.asset = ?")
    .apply(asset.id).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    columns
    .join(Asset.rowVolume(slot.volume), "slot_asset.asset = asset.id")
    .map { (segment, asset) => make(asset, slot.container, segment) }
    .SELECT("WHERE slot_asset.container = ? AND slot_asset.segment && ? ORDER BY slot_asset.segment")
    .apply(slot.containerId, slot.segment).list
}

object AssetSlot extends Table[AssetSlot]("slot_asset") with TableSlot[AssetSlot] {
  private[models] def make(slotAsset : SlotAsset, segment : Segment, consent : Consent.Value, excerpt : Option[Classification.Value]) : AssetSlot =
    slotAsset match {
      case ts : SlotTimeseriesAsset => new TimeseriesAssetSlot(ts, segment, consent, excerpt)
      case f : SlotFileAsset => new FileAssetSlot(f, segment, consent, excerpt)
      case a => new AssetSlot(asset, segment, consent, excerpt)
    }

  private def columnsSegment(seg : Segment) = SlotAsset.columns * Columns(
      SelectColumn[Segment]("asset_slot", "segment")
    )(FromTable("LATERAL (VALUES (slot_asset.segment * ?)) AS asset_slot (segment)"))
    .pushArgs(seg)

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume) which must also be the asset's volume. */
  def get(assetId : Asset.Id, containerId : Container.Id, seg : Segment)(implicit site : Site) : Future[Option[AssetSlot]] =
    columnsSegment(seg)
    .join(Container.columns, "slot_asset.container = container.id")
    .join(Asset.columns, "slot_asset.asset = asset.id")
    .join(Volume.row, "container.volume = volume.id AND asset.volume = volume.id")
    .leftJoin(Excerpt.excerpt, "slot_asset.asset = excerpt.asset AND asset_slot.segment <@ excerpt.segment")
    .leftJoin(SlotConsent.consent, "slot_asset.container = slot_consent.container AND asset_slot.segment <@ slot_consent.segment")
    .map { case ((((((segment, seg), container), asset), volume), excerpt), consent) =>
      make(SlotAsset.make(asset(volume), container(volume), segment), seg, consent, excerpt)
    }
    .SELECT("WHERE asset.id = ? AND container.id = ? AND", Volume.condition)
    .apply(assetId, containerId).singleOpt

  def getAssetFull(asset : Asset) : Future[Option[AssetSlot]] =
    SlotAsset.columns
    .join(Container.columns, "slot_asset.container = container.id")
    .leftJoin(Excerpt.excerpt, "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment")
    .leftJoin(SlotConsent.consent, "slot_asset.container = slot_consent.container AND slot_asset.segment <@ slot_consent.segment")
    .map { case (((segment, container), excerpt), consent) =>
      make(SlotAsset.make(asset, container(asset.volume), segment), segment, consent, excerpt)
    }
    .SELECT("WHERE slot_asset.asset = ? AND container.volume = ?")
    .apply(asset.id, asset.volumeId).singleOpt

  def getFull(slotAsset : SlotAsset) : Future[AssetSlot] =
    SlotAsset.columns
    .leftJoin(Excerpt.excerpt, "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment")
    .leftJoin(SlotConsent.consent, "slot_asset.container = slot_consent.container AND slot_asset.segment <@ slot_consent.segment")
    .map { case ((segment, excerpt), consent) =>
      make(slotAsset, segment, consent, excerpt)
    }
    .SELECT("WHERE slot_asset.asset = ?")
    .apply(slotAsset.assetId).single
}

object Excerpt extends Table[AssetSlot]("excerpt") with TableSlot[AssetSlot] {
  private[models] val excerpt = Columns(
    SelectColumn[Classification.Value]("classification")
  )
  private[models] val columns = excerpt +~ segment

  private def rowVolume(volume : Volume) =
    columns
    .join(SlotAsset.columns, "excerpt.asset = slot_asset.asset")
    .join(Asset.rowFileVolume(volume), "slot_asset.asset = asset.id AND asset.sha1 IS NOT NULL")
    .join(Container.columns, "slot_asset.container = container.id AND volume.id = container.volume")
    .leftJoin(SlotConsent.consent, "slot_asset.container = slot_consent.container AND excerpt.segment <@ slot_consent.segment")
    .map { case (((((excerpt, seg), segment), container), asset), consent) =>
      AssetSlot.make(SlotAsset.make(asset, container(volume), segment), seg, consent, Some(excerpt))
    }

  /** Retrieve the list of all excerpts on a volume. */
  private[models] def getVolume(vol : Volume) : Future[Seq[AssetSlot]] =
    rowVolume(vol)
    .SELECT.apply.list

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getVolumeThumb(vol : Volume) : Future[Option[FileAssetSlot]] =
    selectVolume(vol)
    .SELECT("JOIN format ON asset.format = format.id",
      "WHERE container.volume = ? AND asset.volume = ?",
        "AND GREATEST(excerpt.classification, asset.classification) >= read_classification(?::permission, excerpt_consent.consent)",
        "AND asset.sha1 IS NOT NULL AND (asset.duration IS NOT NULL AND format.mimetype LIKE 'video/%' OR format.mimetype LIKE 'image/%')",
      "ORDER BY container.top DESC LIMIT 1")
    .apply(vol.permission).singleOpt

  /** Retrieve the list of all excerpts in a slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[AssetSlot]] =
    columns
    .join(SlotAsset.columns, "excerpt.asset = slot_asset.asset")
    .join(Asset.rowVolume(slot.volume), "slot_asset.asset = asset.id")
    .leftJoin(SlotConsent.consent, "slot_asset.container = slot_consent.container AND excerpt.segment <@ slot_consent.segment")
    .map { case ((((excerpt, seg), segment), asset), consent) =>
      AssetSlot.make(SlotAsset.make(asset, slot.container, segment), seg, consent, Some(excerpt))
    }
    .SELECT("WHERE asset.volume = ? AND slot_asset.container = ? AND excerpt.segment && ?")
    .apply(slot.volumeId, slot.containerId, slot.segment).list

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

