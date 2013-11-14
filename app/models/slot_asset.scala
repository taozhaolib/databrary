package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an ContainerAsset within the context of a Slot. */
sealed class SlotAsset protected (val link : ContainerAsset, val slot : Slot, excerpt_ : Option[Boolean] = None) extends TableRow with SiteObject with BackedAsset with InVolume {
  def slotId = slot.id
  def volume = link.volume
  def asset = link.asset
  def assetId = link.assetId
  def source = link.asset.source
  def format = asset.format
  private var _excerpt = excerpt_ /* TODO: make this a cached future to simplify queries (and verify selective use) */
  /** Whether this clip has been vetted for public release, permissions permitting. */
  def excerpt : Boolean = _excerpt.getOrElse(false)
  /** Whether if this clip has been promoted for toplevel display. */
  def toplevel : Boolean = _excerpt.isDefined
  def position : Option[Offset] =
    (for { s <- slot.segment.lowerBound ; l <- link.position }
      yield ((l - s).max(0))).
      orElse(link.position)
  def duration : Offset =
    (for { s0 <- slot.segment.lowerBound ; s1 <- slot.segment.upperBound; l <- link.position }
      yield ((s1 - l.max(s0)).min(link.duration))).
      getOrElse(link.duration)

  /** Update the given values in the database and this object in-place.
    * @param excerpt changes both toplevel (`None` or `Some(false)`) and excerpt (`Some(true)`)
    */
  def change(excerpt : Option[Boolean] = _excerpt) : Future[Boolean] = {
    if (excerpt == _excerpt)
      return Async(true)
    val ids = SQLTerms('slot -> slotId, 'asset -> assetId)
    excerpt.fold {
      Audit.remove("toplevel_asset", ids)
    } { e =>
      Audit.changeOrAdd("toplevel_asset", SQLTerms('excerpt -> e), ids)
    }.execute.andThen { case scala.util.Success(true) =>
      _excerpt = excerpt
    }
  }

  def classification = {
    val c = asset.classification
    if (c == Classification.IDENTIFIED && excerpt)
      Classification.EXCERPT
    else
      c
  }

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  override def getPermission : Permission.Value =
    slot.dataPermission(classification).getPermission

  def pageName = link.name
  def pageParent = if(slot.container.top) { Some(slot.volume) } else { Some(slot) }
  def pageURL = controllers.routes.Asset.view(volume.id, slotId, assetId)
  def pageActions = Seq(
      Action("view", controllers.routes.Asset.view(volumeId, slotId, assetId), Permission.VIEW),
      Action("edit", controllers.routes.Asset.edit(volumeId, slotId, assetId), Permission.EDIT)
    ) ++ (if (slot.isFull) Some(
      Action("remove", controllers.routes.Asset.remove(volumeId, slotId, assetId), Permission.CONTRIBUTE)
    ) else None)
}

final class SlotTimeseries private[models] (override val link : ContainerTimeseries, slot : Slot, excerpt_ : Option[Boolean] = None) extends SlotAsset(link, slot, excerpt_) with TimeseriesData {
  override def source = link.asset.source
  def entire = link.asset.entire && link.position.fold(true) { l =>
    slot.segment.lowerBound.fold(true)(_ <= l) &&
    slot.segment.upperBound.fold(true)(_ >= l + link.asset.duration)
  }
  def segment = {
    /* We need to determine the portion of this asset and the slot which overlap, in asset-source space: */
    val b = link.asset.segment /* it must be within (and default to) this asset's own space */
    val a0 = b.lowerBound.get
    val a1 = b.upperBound.get
    val t0 = (for { s0 <- slot.segment.lowerBound ; p <- link.position }
      yield (a0 + (s0 - p).max(0))). /* shifted forward if the slot starts later than the asset */
      getOrElse(a0)
    val t1 = (for { s1 <- slot.segment.upperBound ; p <- link.position }
      yield ((a0 + s1 - p).min(a1))). /* the lesser of the slot end and the asset end */
      getOrElse(a1)
    Range[Offset](t0, t1)
  }
  override def duration : Offset = super[SlotAsset].duration
  override def format =
    if (slot.segment.isSingleton) link.asset.format match {
      case t : TimeseriesFormat => t.sampleFormat
      case f => f
    } else link.asset.format
}

object SlotAsset extends Table[SlotAsset]("toplevel_asset") {
  private def make(link : ContainerAsset, slot : Slot, excerpt : Option[Boolean]) = link match {
    case ts : ContainerTimeseries => new SlotTimeseries(ts, slot, excerpt)
    case _ => new SlotAsset(link, slot, excerpt)
  }
  private val columns = Columns(
      SelectColumn[Boolean]("excerpt")
    )
  private def condition(segment : String = "slot.segment") =
    "(container_asset.position IS NULL OR container_asset.position <@ " + segment +
    " OR segment_shift(segment(" + Asset.duration + "), container_asset.position) && " + segment +
    ")"
  private val classification = "CASE WHEN toplevel_asset.excerpt AND file.classification = 'IDENTIFIED' THEN 'EXCERPT' else file.classification"
  private def volumeRow(vol : Volume) = ContainerAsset.containerColumns.
    join(Container.volumeRow(vol), "container_asset.container = container.id").
    join(Slot.columns, "container.id = slot.source").
    leftJoin(columns, "slot.id = toplevel_asset.slot AND asset.id = toplevel_asset.asset") map {
      case (((link, cont), slot), excerpt) => make(link(cont), slot(cont), excerpt)
    }
  private def slotRow(slot : Slot) = ContainerAsset.containerRow(slot.container).
    leftJoin(columns, "asset.id = toplevel_asset.asset AND toplevel_asset.slot = ?") map {
      case (link, excerpt) => make(link, slot, excerpt)
    }

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume).
    * @param full only return full slots */
  def get(asset : Asset.Id, slot : Slot.Id, full : Boolean = false)(implicit site : Site) : Future[Option[SlotAsset]] =
    ContainerAsset.row
      .join(Slot.columns, "container.id = slot.source")
      .leftJoin(columns, "slot.id = toplevel_asset.slot AND asset.id = toplevel_asset.asset")
      .map {
        case ((link, slot), excerpt) => make(link, slot(link.container), excerpt)
      }
      .SELECT("WHERE slot.id = ? AND asset.id = ?",
        if (full) "AND slot.segment = '(,)'" else "",
        "AND", condition(), "AND", Volume.condition)
      .apply(SQLArgs(slot, asset) ++ Volume.conditionArgs).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    slotRow(slot).SELECT("WHERE container_asset.container = ? AND", condition("?::segment"), "ORDER BY container_asset.position NULLS LAST, format.id")
      .apply(slot.id, slot.containerId, slot.segment, slot.segment).list

  /** Build the SlotAsset for the given ContainerAsset#container.fullSlot. */
  private[models] def getFull(ca : ContainerAsset) : Future[SlotAsset] =
    ca.container._fullSlot.peek.fold {
      Slot.containerRow(ca.container)
        .leftJoin(columns, "slot.id = toplevel_asset.slot AND toplevel_asset.asset = ?")
        .map { case (slot, excerpt) =>
          ca.container._fullSlot.set(slot)
          make(ca, slot, excerpt)
        }.SELECT("WHERE slot.source = ? AND slot.segment = '(,)'")
        .apply(ca.assetId, ca.containerId).single
    } { slot => 
      columns.SELECT("WHERE toplevel_asset.slot = ? AND toplevel_asset.asset = ?")
        .apply(slot.id, ca.assetId).singleOpt
        .map(make(ca, slot, _))
    }

  /** Retrieve the list of all top-level assets. */
  private[models] def getToplevel(volume : Volume) : Future[Seq[SlotAsset]] =
    for {
      l <- volumeRow(volume).SELECT("WHERE toplevel_asset.excerpt IS NOT NULL AND container.volume = ? AND",
          condition(),
          "ORDER BY toplevel_asset.excerpt DESC")
        .apply(volume.id).list
      s <- volume.topSlot
      t <- getSlot(s)
    } yield(l ++ t)

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getThumb(volume : Volume)(implicit site : Site) : Future[Option[SlotAsset]] =
    volumeRow(volume).SELECT("""
      WHERE (toplevel_asset.excerpt IS NOT NULL OR container.top AND slot.segment = '(,)' OR slot.consent >= 'PRIVATE')
        AND (format.id = ? OR format.mimetype LIKE 'image/%')
        AND data_permission(?::permission, slot_consent(slot.id), file.classification, ?::permission, toplevel_asset.excerpt) >= 'DOWNLOAD'
        AND container.volume = ?
        AND""", condition(), " ORDER BY toplevel_asset.excerpt DESC NULLS LAST, container.top DESC, slot.consent DESC NULLS LAST LIMIT 1")
      .apply(TimeseriesFormat.VIDEO, volume.getPermission, site.access, volume.id).singleOpt

  /** Find an asset suitable for use as a slot thumbnail. */
  private[models] def getThumb(slot : Slot)(implicit site : Site) : Future[Option[SlotAsset]] =
    slotRow(slot).SELECT("""
      WHERE container_asset.container = ?
        AND (format.id = ? OR format.mimetype LIKE 'image/%') 
        AND data_permission(?::permission, ?::consent, file.classification, ?::permission, toplevel_asset.excerpt) >= 'DOWNLOAD'
        AND""", condition("?::segment"), "LIMIT 1")
      .apply(slot.id, slot.containerId, TimeseriesFormat.VIDEO, slot.getPermission, slot.consent, site.access, slot.segment, slot.segment).singleOpt
}
