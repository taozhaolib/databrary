package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import macros.async._
import dbrary._
import site._

private[models] final case class Excerpt(classification : Classification.Value, segment : Segment)

sealed trait AssetSlot extends Slot {
  def slotAsset : SlotAsset
  val excerpt : Option[Excerpt]
  def asset : Asset = slotAsset.asset
  final def assetId = asset.id
  override final def volume = asset.volume
  protected final def assetSegment = slotAsset.segment
  def format = asset.format
  final def classification = excerpt.fold(asset.classification)(max(asset.classification, _))

  private[models] def withConsent(consent : Consent.Value) =
    if (consent == this.consent) this else
    AssetSlot(slotAsset, segment, consent, excerpt)

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

  protected def json : JsonObject = JsonObject.flatten(
    Some('permission -> permission),
    if (segment.isFull) None else Some('segment -> segment),
    if (consent == Consent.NONE) None else Some('consent -> consent), // probably no-one uses this
    excerpt.map('excerpt -> _)
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
sealed class AssetSegment protected (val slotAsset : SlotAsset, _segment : Segment, final val context : ContextSlot, final val excerpt : Option[Excerpt])
  extends AssetSlot {
  final val segment = slotAsset.segment * _segment
  private[models] def sqlKey = SQLTerms('asset -> assetId, 'segment -> segment)

  override def pageName = _segment.toString
  override def pageParent = Some(slotAsset)
    
  override def json : JsonObject = super.json +
    ('asset -> slotAsset.json) ++
    (if (format === asset.format) None else Some('format -> format.id))
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
    columns
    .join(ContextSlot.rowContainer(
        Container.columnsVolume(Volume.fixed(asset.volume)),
        "slot_asset.segment"),
      "slot_asset.container = container.id")
    .leftJoin(Excerpt.columns, "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment")
    .map { case ((segment, context), excerpt) =>
      SlotAsset(asset, segment, context, excerpt)
    }
    .SELECT("WHERE slot_asset.asset = ?")
    .apply(asset.id).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    columns
    .join(Asset.rowVolume(slot.volume), "slot_asset.asset = asset.id")
    .join(ContextSlot.rowContainer(slot.container, "slot_asset.segment"),
      "slot_asset.container = container.id")
    .leftJoin(Excerpt.columns, "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment")
    .map { case (((segment, asset), context), excerpt) =>
      SlotAsset(asset, segment, context, excerpt)
    }
    .SELECT("WHERE slot_asset.segment && ? ORDER BY slot_asset.segment")
    .apply(slot.segment).list
}

object AssetSlot extends Table[AssetSlot]("slot_asset") with TableSlot[AssetSlot] {
  private[models] def apply(slotAsset : SlotAsset, segment : Segment, context : CnotextSlot, excerpt : Option[Classification.Value]) : AssetSlot =
    if (segment === slotAsset.segment) slotAsset else slotAsset match {
      case ts : SlotTimeseriesAsset => new TimeseriesAssetSegment(ts, segment, context, excerpt)
      case f : SlotFileAsset => new FileAssetSegment(f, segment, context, excerpt)
      case a => new AssetSegment(a, segment, context, excerpt)
    }

  private def columnsSegment(seg : Segment) = SlotAsset.columns * Columns(
      SelectColumn[Segment]("asset_slot", "segment")
    )(FromTable("LATERAL (VALUES (slot_asset.segment * ?)) AS asset_slot (segment)"))
    .pushArgs(SQLArgs(seg))

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
      AssetSlot(SlotAsset(asset(volume), container(volume), segment), seg, consent.getOrElse(Consent.NONE), excerpt)
    }
    .SELECT("WHERE asset.id = ? AND container.id = ? AND", Volume.condition)
    .apply(assetId, containerId).singleOpt

  def getFull(slotAsset : SlotAsset) : Future[AssetSlot] =
    SlotAsset.columns
    .leftJoin(Excerpt.excerpt, "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment")
    .leftJoin(SlotConsent.consent, "slot_asset.container = slot_consent.container AND slot_asset.segment <@ slot_consent.segment")
    .map { case ((segment, excerpt), consent) =>
      AssetSlot(slotAsset, segment, consent.getOrElse(Consent.NONE), excerpt)
    }
    .SELECT("WHERE slot_asset.asset = ?")
    .apply(slotAsset.assetId).single

  def getSlotFull(slot : Slot) : Future[Seq[AssetSlot]] =
    SlotAsset.columns
    .join(Asset.columns, "slot_asset.asset = asset.id")
    .leftJoin(Excerpt.excerpt, "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment")
    .leftJoin(SlotConsent.consent, "slot_asset.container = slot_consent.container AND slot_asset.segment <@ slot_consent.segment")
    .map { case (((segment, asset), excerpt), consent) =>
      AssetSlot(SlotAsset(asset(slot.volume), slot.container, segment), segment, consent.getOrElse(Consent.NONE), excerpt)
    }
    .SELECT("WHERE slot_asset.container = ? AND slot_asset.segment && ? AND asset.volume = ?")
    .apply(slot.containerId, slot.segment, slot.volumeId).list
}

object Excerpt extends Table[AssetSlot]("excerpt") with TableSlot[AssetSlot] {
  private[models] val excerpt = Columns(
    SelectColumn[Classification.Value]("classification")
  )
  private[models] val columns =
    (excerpt ~+ segment).map(Excerpt.apply)

  private def rowVolume(volume : Volume) =
    columns
    .join(SlotAsset.columns, "excerpt.asset = slot_asset.asset")
    .join(Asset.rowFileVolume(volume), "slot_asset.asset = asset.id AND asset.sha1 IS NOT NULL")
    .join(Container.columns, "slot_asset.container = container.id AND volume.id = container.volume")
    .leftJoin(SlotConsent.consent, "slot_asset.container = slot_consent.container AND excerpt.segment <@ slot_consent.segment")
    .map { case (((((excerpt, seg), segment), asset), container), consent) =>
      AssetSlot(SlotAsset(asset, container(volume), segment), seg, consent.getOrElse(Consent.NONE), Some(excerpt)).asInstanceOf[FileAssetSlot]
    }

  /** Retrieve the list of all excerpts on a volume. */
  private[models] def getVolume(vol : Volume) : Future[Seq[FileAssetSlot]] =
    rowVolume(vol)
    .SELECT().apply().list

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getVolumeThumb(vol : Volume) : Future[Option[FileAssetSlot]] =
    rowVolume(vol)
    .SELECT("JOIN format ON asset.format = format.id",
      "WHERE GREATEST(excerpt.classification, asset.classification) >= read_classification(?::permission, slot_consent.consent)",
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
      AssetSlot(SlotAsset(asset, slot.container, segment), seg, consent.getOrElse(Consent.NONE), Some(excerpt))
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

