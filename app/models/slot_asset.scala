package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import macros.async._
import dbrary._
import site._

sealed trait AssetSlot extends Slot {
  def slotAsset : SlotAsset
  val excerpt : Option[Classification]

  def asset : Asset = slotAsset.asset
  final def assetId = asset.id
  override final def volume = asset.volume
  final def entire = segment === slotAsset.segment
  def format = asset.format
  protected def assetSegment = soltAsset.segment

  final def classification = excerpt.fold(asset.classification)(max(asset.classification, _))

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  final override val permission : Permission.Value =
    slot.dataPermission(classification).permission
    
  final override def auditDownload(implicit site : Site) : Future[Boolean] =
    Audit.download("slot_asset", 'container -> containerId, 'segment -> segment, 'asset -> assetId)

  override def pageName = asset.pageName
  override def pageParent = Some(slot)
  override def pageURL = controllers.routes.SlotAssetHtml.view(containerId, slot.segment, assetId)

  override def json : JsonObject = JsonObject.flatten(
    Some('permission -> permission),
    excerpt.map('excerpt -> _),
    Some('asset -> slotAsset.json),
    if (format === asset.format) None else Some('format -> format.id)
  ) ++ (slotJson - "container")

  override def fileName : Future[String] =
    for {
      vol <- volume.fileName
      slot <- super.fileName
    } yield {
      store.fileName(Seq(vol) ++ Maybe(slot).opt ++ asset.name.map(store.truncate(_)) : _*)
    }
}

sealed trait FileAssetSlot extends AssetSlot with BackedAsset {
  def slotAsset : SlotFileAsset
  def asset : FileAsset = slotAsset.asset
  def source = asset.source
}

sealed trait TimeseriesAssetSlot extends FileAssetSlot with TimeseriesData {
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
sealed class SlotAsset protected (override val asset : Asset, val container : Container, val segment : Segment, val consent : Consent.Value, val excerpt : Option[Classification])
  extends AssetSlot {
  private[models] final def sqlKey = asset.sqlKey
  def slotAsset = this

  override def json : JsonObject =
    asset.json ++ slotJson 
}

sealed class SlotFileAsset protected (override val asset : FileAsset, container : Container, segment : Segment, consent : Consent.Value, excerpt : Option[Classification])
  extends SlotAsset(asset, container, segment, consent, excerpt) with FileAssetSlot

sealed class SlotTimeseriesAsset protected (override val asset : TimeseriesAsset, container : Container, segment : Segment, consent : Consent.Value, excerpt : Option[Classification])
  extends SlotFileAsset(asset, container, segment, consent, excerpt) with TimeseriesAssetSlot

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an SlotAsset within the context of a segment. */
sealed class SlotAssetSegment protected (val slotAsset : SlotAsset, val segment : Segment, val consent : Consent.Value, val excerpt : Option[Classification])
  extends AssetSlot

sealed class SlotFileAssetSegment private[models] (override val slotAsset : SlotFileAsset, segment : Segment, consent : Consent.Value, excerpt : Option[Classification])
  extends SlotAssetSegment(slotAsset, segment, consent, excerpt) with FileAssetSlot

final class SlotTimeseriesAssetSegment private[models] (override val slotAsset : SlotTimeseriesAsset, segment : Segment, consent : Consent.Value, excerpt : Option[Classification])
  extends SlotFileAssetSegment(slotAsset, segment, consent, excerpt) with TimeseriesAssetSlot


object SlotAsset extends Table[SlotAsset]("slot_asset") with TableSlot[SlotAsset] {
  private[models] def make(segment : Segment, excerpt : Option[Classification])(asset : Asset)(container : Container, consent : Consent.Value) : SlotAsset = asset match {
    case ts : TimeseriesAsset => new SlotTimeseriesAsset(ts, container, segment, consent, excerpt)
    case f : FileAsset => new SlotFileAsset(f, container, segment, consent, excerpt)
    case a => new SlotAsset(a, container, segment, consent, excerpt)
  }

  private val columns : Selector[Asset => (Container, Consent.Value) => SlotAsset] =
    Columns(segment)
    .leftJoin(Excerpt.excerpt, "slot_asset.asset = excerpt.excerpt AND slot_asset.segment <@ excerpt.segment")
    .map { (segment, excerpt) => make(segment, excerpt) }

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    columnsSlot(columns
      .join(Asset.rowVolume(slot.volume), "slot_asset.asset = asset.id")
      .map(tupleApply)
      slot.container)
    .SELECT("WHERE slot_asset.segment && ? ORDER BY slot_asset.segment")
    .apply(slot.segment).list

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    columnsSlot(columns.map(_(asset)),
      Container.columnsVolume(asset.volume))
    .SELECT("WHERE slot_asset.asset = ?")
    .apply(asset.id).singleOpt
}

object SlotAssetSegment extends Table[SlotAssetSegment]("slot_asset") {
  private[models] def make(segment : Segment, excerpt : Option[Classification])(slotAsset : Asset)(container : Container, consent : Consent.Value) = slotAsset match {
    case ts : SlotTimeseriesAsset => new SlotTimeseriesAssetSegment(ts, slot, excerpt)
    case f : SlotFileAsset => new SlotFileAssetSegment(f, slot, excerpt)
    case a => new SlotAssetSegment(asset, slot, excerpt)
  }

  private object SlotAssetSlot extends SlotTable("slot_asset") {
  }

  private def row(slot : Selector[Slot], asset : Selector[Volume => Asset] = Asset.columns) : Selector[SlotAsset] =
    slot
    .join(SlotAssetSlot.columns, "slot.container = slot_asset.container AND slot.segment && slot_asset.segment")
    .leftJoin(Excerpt.columns, "slot.segment <@ excerpt.segment AND slot_asset.asset = excerpt.asset")
    .join(asset, "slot_asset.asset = asset.id")
    .map { case (((slot, segment), excerpt), asset) =>
      make(asset(slot.volume), segment, slot, excerpt.map(_(slot.context)))
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
    .SELECT("WHERE asset.volume = ? ORDER BY slot.segment")
    .apply(slot.volumeId).list

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    SlotAssetSlot.rowVolume(Volume.fixed(asset.volume))
    .leftJoin(Excerpt.columns, "slot_asset.segment <@ excerpt.segment AND slot_asset.asset = excerpt.asset")
    .map { case (slot, excerpt) =>
      make(asset, slot.segment, slot, excerpt.map(_(slot.context)))
    }
    .SELECT("WHERE slot_asset.asset = ?")
    .apply(asset.id).singleOpt
}

object Excerpt extends Table[Excerpt]("excerpt") with TableSlot[Excerpt] {
  protected final type A = Excerpt
  private[models] val excerpt = Columns(
      SelectColumn[Classification.Value]("classification")
    )
  private[models] val columns = (excerpt +~ segment)
    .map { (classification, segment) => (container, consent) =>
      new SlotAssetSlot(segment, classification, context)
    }
  private[models] val columnsContext = columns from
    "(SELECT asset, container, excerpt.segment, excerpt.classification, slot_asset.segment AS asset_segment FROM slot_asset JOIN excerpt USING (asset)) AS excerpt"

  private def selectSlot(slot : Slot) = 
    columnsSlot(columnsContext, slot.container)
    .~(SelectColumn[Segment]("excerpt", "asset_segment"))
    .join(Asset.columns, "excerpt.asset = asset.id")
    .map { case ((excerpt, segment), asset) =>
      SlotAsset.make(asset(slot.volume), segment, excerpt, Some(excerpt))
    }

  private def selectVolume(vol : Volume) = 
    columnsSlot(columnsContext, Container.columnsVolume(Volume.fixed(vol)))
    .~(SelectColumn[Segment]("excerpt", "asset_segment"))
    .join(Asset.columns, "excerpt.asset = asset.id AND container.volume = asset.volume")
    .map { case ((excerpt, segment), asset) =>
      SlotAsset.make(asset(vol), segment, excerpt, Some(excerpt))
    }

  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    selectSlot(slot)
    .SELECT("WHERE asset.volume = ? AND excerpt.container = ? AND excerpt.segment && ?")
    .apply(slot.volumeId, slot.containerId, slot.segment).list

  /** Retrieve the list of all readable excerpts. */
  private[models] def getVolume(vol : Volume) : Future[Seq[SlotAsset]] =
    selectVolume(vol)
    .SELECT().apply().list

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getVolumeThumb(vol : Volume) : Future[Option[SlotAsset]] =
    selectVolume(vol)
    .SELECT("JOIN format ON asset.format = format.id",
      "WHERE GREATEST(excerpt.classification, asset.classification) >= read_classification(?::permission, excerpt_consent.consent)",
        "AND (asset.duration IS NOT NULL AND format.mimetype LIKE 'video/%' OR format.mimetype LIKE 'image/%')",
      "ORDER BY container.top DESC LIMIT 1")
    .apply(vol.permission).singleOpt

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

