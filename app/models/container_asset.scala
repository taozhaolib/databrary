package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** An embedding or link (in the filesystem sense) of an asset within a container.
  * An asset link includes the asset and container, along with a name and description for that particular link.
  * Permissions are checked in msot cases, as indicated.
  */
sealed class ContainerAsset protected (val asset : Asset, val container : Container, position_ : Option[Offset], name_ : String, body_ : Option[String]) extends TableRow with SitePage with InVolume {
  def assetId = asset.id
  def containerId = container.id
  def id = (assetId, containerId)
  def volume = container.volume
  private[this] var _position = position_
  /** Start point of this asset within the container. */
  def position : Option[Offset] = _position
  private[this] var _name = name_
  /** Title or name of the asset as used in the container. */
  def name : String = _name
  private[this] var _body = body_
  /** Optional description of this asset. */
  def body : Option[String] = _body

  /** Update the given values in the database and this object in-place. */
  def change(position : Option[Offset] = _position, name : String = _name, body : Option[String] = _body)(implicit site : Site) : Future[Boolean] = {
    if (position == _position && name == _name && body == _body)
      return Async(false)
    Audit.change("container_asset", SQLTerms('position -> position, 'name -> name, 'body -> body), SQLTerms('container -> containerId, 'asset -> assetId))
      .andThen { case Success(true) =>
        _name = name
        _body = body
      }
  }

  def remove : Future[Boolean] =
    Audit.remove("container_asset", SQLTerms('container -> containerId, 'asset -> assetId)).execute

  def duration : Offset = 0
  /** Range of times that this asset covers, or None for "global/floating". */
  def extent : Option[Range[Offset]] = position.map(Range.singleton[Offset](_))

  def fullSlot : Future[SlotAsset] = SlotAsset.getFull(this)

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL(implicit site : Site) = controllers.routes.Asset.view(volume.id, container._fullSlot.get.id, assetId)
  def pageActions(implicit site : Site) = Seq(
    Action("view", controllers.routes.Asset.view(volumeId, container._fullSlot.get.id, assetId), Permission.VIEW),
    Action("edit", controllers.routes.Asset.edit(volumeId, containerId, assetId), Permission.EDIT),
    Action("remove", controllers.routes.Asset.remove(volumeId, containerId, assetId), Permission.CONTRIBUTE)
  )
}

final class ContainerTimeseries private[models] (override val asset : Asset with TimeseriesData, container : Container, position_ : Option[Offset], name_ : String, body_ : Option[String]) extends ContainerAsset(asset, container, position_, name_, body_) {
  override def duration : Offset = asset.duration
  override def extent : Option[Range[Offset]] = position.map(start =>
    Range[Offset](start, start + duration))
}

object ContainerAsset extends Table[ContainerAsset]("container_asset") {
  private[models] def make(asset : Asset, container : Container)(position : Option[Offset], name : String, body : Option[String]) = asset match {
    case ts : TimeseriesData => new ContainerTimeseries(ts, container, position, name, body)
    case _ => new ContainerAsset(asset, container, position, name, body)
  }
  private[models] val columns = Columns(
      SelectColumn[Option[Offset]]("position")
    , SelectColumn[String]("name")
    , SelectColumn[Option[String]]("body")
    )
  private[models] val containerColumns = columns.join(Asset.row, "container_asset.asset = asset.id")
  private[models] def containerRow(cont : Container) = containerColumns map {
    case (link, asset) => (make(asset, cont) _).tupled(link)
  }
  private[models] val row = containerColumns.join(Container.row, "container_asset.container = container.id") map {
    case ((link, asset), cont) => (make(asset, cont) _).tupled(link)
  }

  /** Retrieve a specific asset link by asset id and container id.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  def get(asset : Asset.Id, container : Container.Id)(implicit site : Site) : Future[Option[ContainerAsset]] =
    row.SELECT("WHERE container_asset.asset = ? AND container_asset.container = ? AND", Volume.condition)
      .apply(SQLArgs(asset, container) ++ Volume.conditionArgs).singleOpt

  /** Retrieve a specific asset link by asset.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  private[models] def get(asset : Asset)(implicit site : Site) : Future[Option[ContainerAsset]] =
    columns.join(Container.row, "container_asset.container = container.id").map {
        case (link, cont) => (make(asset, cont) _).tupled(link)
      }
      .SELECT("WHERE container_asset.asset = ? AND", Volume.condition)
      .apply(asset.id +: Volume.conditionArgs).singleOpt

  /** Retrieve a specific asset link by container and asset id.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def get(container : Container, asset : Asset.Id) : Future[Option[ContainerAsset]] =
    containerRow(container)
      .SELECT("WHERE container_asset.container = ? AND container_asset.asset = ?")
      .apply(container.id, asset).singleOpt

  /** Retrieve the set of assets directly contained by a single container.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def getContainer(container : Container) : Future[Seq[ContainerAsset]] =
    containerRow(container)
      .SELECT("WHERE container_asset.container = ? ORDER BY container_asset.position NULLS LAST, format.id")
      .apply(container.id).list

  /** Find the assets in a container with the given name. */
  def findName(container : Container, name : String) : Future[Seq[ContainerAsset]] =
    containerRow(container)
      .SELECT("WHERE container_asset.container = ? AND container_asset.name = ?")
      .apply(container.id, name).list

  /** Create a new link between an asset and a container.
    * This can change effective permissions on this asset, so care must be taken when using this function with existing assets. */
  def create(container : Container, asset : Asset, position : Option[Offset] = None, name : String, body : Option[String] = None)(implicit site : Site) : Future[ContainerAsset] = {
    Audit.add(table, SQLTerms('container -> container.id, 'asset -> asset.id, 'position -> position, 'name -> name, 'body -> body)).map { _ =>
      new ContainerAsset(asset, container, position, name, body)
    }
  }
}

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an ContainerAsset within the context of a Slot. */
sealed class SlotAsset protected (val link : ContainerAsset, val slot : Slot, excerpt_ : Option[Boolean] = None) extends SitePage with BackedAsset with InVolume {
  def slotId = slot.id
  def volume = link.volume
  def source = link.asset.source
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
  def change(excerpt : Option[Boolean] = _excerpt)(implicit site : Site) : Unit = {
    if (excerpt != _excerpt)
    {
      val ids = SQLTerms('slot -> slotId, 'asset -> link.assetId)
      excerpt.fold {
        Audit.remove("toplevel_asset", ids).run()
      } { e =>
        Audit.changeOrAdd("toplevel_asset", SQLTerms('excerpt -> e), ids).run()
      }
      _excerpt = excerpt
    }
  }

  def format = link.asset.format

  def classification = {
    val c = link.asset.classification
    if (c == Classification.IDENTIFIED && excerpt)
      Classification.EXCERPT
    else
      c
  }

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  override def getPermission(implicit site : Site) : Permission.Value =
    slot.dataPermission(classification).getPermission

  def pageName(implicit site : Site) = link.name
  def pageParent(implicit site : Site) = if(slot.container.top) { Some(slot.volume) } else { Some(slot) }
  def pageURL(implicit site : Site) = controllers.routes.Asset.view(volume.id, slotId, link.assetId)
  def pageActions(implicit site : Site) =
    if (slot.isFull) link.pageActions
    else Seq(
      Action("view", controllers.routes.Asset.view(volumeId, slotId, link.assetId), Permission.VIEW),
      Action("edit", controllers.routes.Asset.edit(volumeId, link.containerId, link.assetId), Permission.EDIT)
    )
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
      case (link ~ asset ~ cont ~ slot ~ excerpt) => make((ContainerAsset.make(asset, cont) _).tupled(link), (Slot.make(cont) _).tupled(slot), excerpt)
    }
  private def slotRow(slot : Slot) = ContainerAsset.containerRow(slot.container).
    leftJoin(columns, "asset.id = toplevel_asset.asset AND toplevel_asset.slot = ?") map {
      case (link ~ excerpt) => make(link, slot, excerpt)
    }

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume). */
  def get(asset : Asset.Id, slot : Slot.Id)(implicit site : Site) : Future[Option[SlotAsset]] =
    ContainerAsset.row
      .join(Slot.columns, "container.id = slot.source")
      .leftJoin(columns, "slot.id = toplevel_asset.slot AND asset.id = toplevel_asset.asset") map {
        case ((link, slot), excerpt) => make(link, (Slot.make(link.container) _).tupled(slot), excerpt)
      }
      .SELECT("WHERE slot.id = ? AND asset.id = ? AND", condition(), "AND", Volume.condition)
      .apply(SQLArgs(slot, asset) ++ Volume.conditionArgs).singleOpt

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[SlotAsset]] =
    slotRow(slot).SELECT("WHERE container_asset.container = ? AND", condition("?"), "ORDER BY container_asset.position NULLS LAST, format.id")
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
    volumeRow(volume).SELECT("WHERE toplevel_asset.excerpt IS NOT NULL AND container.volume = ? AND",
        condition(),
        "ORDER BY toplevel_asset.excerpt DESC")
      .apply(volume.id).list
      .flatMap { l =>
        getSlot(volume.topSlot)
          .map(l ++ _)
      }

  /** Find an asset suitable for use as a volume thumbnail. */
  private[models] def getThumb(volume : Volume)(implicit site : Site) : Future[Option[SlotAsset]] =
    volumeRow(volume).SELECT("""
      WHERE (toplevel_asset.excerpt IS NOT NULL OR container.top AND slot.segment = '(,)' OR slot.consent >= 'PRIVATE')
        AND (format.id = ? OR format.mimetype LIKE 'image/%')
        AND data_permission(?, slot_consent(slot.id), file.classification, ?, toplevel_asset.excerpt) >= 'DOWNLOAD'
        AND container.volume = ?
        AND""", condition(), " ORDER BY toplevel_asset.excerpt DESC NULLS LAST, container.top DESC, slot.consent DESC NULLS LAST LIMIT 1")
      .apply(TimeseriesFormat.VIDEO, volume.getPermission, site.access, volume.id).singleOpt

  /** Find an asset suitable for use as a slot thumbnail. */
  private[models] def getThumb(slot : Slot)(implicit site : Site) : Future[Option[SlotAsset]] =
    slotRow(slot).SQL("""
      WHERE container_asset.container = ?
        AND (format.id = ? OR format.mimetype LIKE 'image/%') 
        AND data_permission(?, ?, file.classification, ?, toplevel_asset.excerpt) >= 'DOWNLOAD'
        AND""", condition("?"), "LIMIT 1")
      .apply(slot.id, slot.containerId, TimeseriesFormat.VIDEO, slot.getPermission, slot.consent, site.access, slot.segment, slot.segment).singleOpt
}
