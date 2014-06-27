package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import macros.async._
import dbrary._
import site._

final class Excerpt (val segment : Segment, val classification : Classification.Value, val context : ContextSlot) extends Slot {
  private[models] def sqlKey = ???
}

object Excerpt extends Table[Excerpt]("excerpt") with TableSlot[Excerpt] {
  protected final type A = Excerpt
  private[models] val columns = Columns(
      segment
    , SelectColumn[Classification.Value]("classification")
    ).map { (segment, classification) =>
      context : ContextSlot => new Excerpt(segment, classification, context)
    }
  private[models] val columnsContext = columns from
    "(SELECT asset, container, excerpt.segment, excerpt.classification, slot_asset.segment AS asset_segment FROM slot_asset JOIN excerpt USING (asset)) AS excerpt"

  private[models] def rowVolume(volume : Selector[Volume]) =
    columnsSlot(columnsContext, Container.columnsVolume(volume))

  def set(asset : Asset, segment : Segment, classification : Option[Classification.Value]) : Future[Boolean] = {
    implicit val site = asset.site
    val key = SQLTerms('asset -> asset.id, 'segment -> segment)
    classification.fold {
      Audit.remove("excerpt", key)
    } { classification =>
      Audit.changeOrAdd("excerpt", SQLTerms('classification -> classification), key)
    }.execute
  }
}

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an ContainerAsset within the context of a Slot. */
sealed class SlotAsset protected (val asset : Asset, asset_segment : Segment, val slot : Slot, excerpt : Option[Excerpt]) extends Slot with TableRow with BackedAsset with InVolume with SiteObject {
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
  require(excerpt.forall(_ @> this))

  final def classification = excerpt.fold(asset.classification)(e => max(e.classification, asset.classification))

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  final override val permission : Permission.Value =
    slot.dataPermission(classification).permission

  final private def in(s : Slot) = {
    require(s.containerId === containerId)
    if (s.segment === slot.segment)
      this
    else
      SlotAsset.make(asset, asset_segment, s, excerpt.filter(_.segment @> s.segment))
  }
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

  def fileName : Future[String] = {
    def last(s : String) =
      Maybe(s.lastIndexOf(' ')).fold(s)(i => s.substring(i+1))
    for {
      owns <- volume.partyAccess(Permission.ADMIN)
      own = owns.headOption.map(_.party.name).map(last _)
      cite <- volume.citation
      auth = cite.flatMap(_.authors).flatMap(_.headOption).map(last _).filterNot(_.equals(own))
      name = { 
	val n = volume.alias.getOrElse(volume.name)
	n.take(Maybe(n.lastIndexOf(' ', 32)).orElse(32))
      }
      ids <- idents
    } yield {
      SlotAsset.fileNamePad.replaceAllIn(
	(Seq("databrary" + volume.id) ++
	own ++ auth ++ cite.flatMap(_.year).map(_.toString) ++
	(name +: (ids ++ asset.name))).mkString("-"),
	"_")
    }
  }

  override def json : JsonObject = JsonObject.flatten(
    Some('permission -> permission),
    if (format === asset.format) None else Some('format -> format.id),
    excerpt.map('excerpt -> _.classification),
    Some('asset -> (asset.json ++
      JsonObject.flatten(if (asset_segment.isFull) None else Some('segment -> asset_segment))))
  ) ++ slotJson

  def json(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(json.obj, options
    )
}

final class SlotTimeseries private[models] (override val asset : TimeseriesAsset, asset_segment : Segment, slot : Slot, _excerpt : Option[Excerpt]) extends SlotAsset(asset, asset_segment, slot, _excerpt) with TimeseriesData {
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
  private[models] def make(asset : Asset, asset_segment : Segment, slot : Slot, excerpt : Option[Excerpt]) = asset match {
    case ts : TimeseriesAsset => new SlotTimeseries(ts, asset_segment, slot, excerpt)
    case _ => new SlotAsset(asset, asset_segment, slot, excerpt)
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
    .SELECT("WHERE asset.volume = ?")
    .apply(slot.volumeId).list

  /** Retrieve an asset's native (full) SlotAsset representing the entire span of the asset. */
  private[models] def getAsset(asset : Asset) : Future[Option[SlotAsset]] =
    row(Slot.rowVolume(Volume.fixed(asset.volume)), Asset.fixed(asset).map(const _))
    .SELECT("WHERE slot.volume = ?")
    .apply(asset.volumeId).singleOpt

  private def excerpts(volume : Volume) = 
    Excerpt.rowVolume(Volume.fixed(volume))
    .~(SelectColumn[Segment]("excerpt", "asset_segment"))
    .join(Asset.columns, "excerpt.asset = asset.id AND container.volume = asset.volume")
    .map { case ((excerpt, segment), asset) =>
      make(asset(volume), segment, excerpt, Some(excerpt))
    }

  /** Retrieve the list of all readable excerpts. */
  private[models] def getExcerpts(volume : Volume) : Future[Seq[SlotAsset]] =
    excerpts(volume)
    .SELECT("WHERE GREATEST(excerpt.classification, asset.classification) >= read_classification(?::permission, excerpt_consent.consent)")
    .apply(volume.permission).list

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getThumb(volume : Volume) : Future[Option[SlotAsset]] =
    excerpts(volume)
    .SELECT("WHERE GREATEST(excerpt.classification, asset.classification) >= read_classification(?::permission, excerpt_consent.consent)",
      "ORDER BY container.top DESC LIMIT 1")
    .apply(volume.permission).singleOpt

  private final val fileNamePad = "[\0-,/?\\\\]+".r
}
