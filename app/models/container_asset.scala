package models

import java.sql.Date
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import PGSegment.{column => segmentColumn,statement => segmentStatement}
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

  private[this] def ids = SQLArgs('container -> containerId, 'asset -> assetId)

  /** Update the given values in the database and this object in-place. */
  def change(position : Option[Offset] = _position, name : String = _name, body : Option[String] = _body)(implicit site : Site) : Unit = {
    if (position == _position && name == _name && body == _body)
      return
    Audit.change("container_asset", SQLArgs('position -> position, 'name -> name, 'body -> body), SQLArgs('container -> containerId, 'asset -> assetId)).execute()
    _name = name
    _body = body
  }

  def remove(implicit site : Site) : Unit =
    Audit.remove("container_asset", SQLArgs('container -> containerId, 'asset -> assetId)).execute

  def duration : Offset = 0
  /** Range of times that this asset covers, or None for "global/floating". */
  def extent : Option[Range[Offset]] = position.map(Range.singleton[Offset](_)(PGSegment))

  def fullSlot(implicit db : Site.DB) : SlotAsset = SlotAsset.getFull(this)

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL(implicit site : Site) = controllers.routes.Asset.view(volume.id, container.fullSlot.id, assetId)
  def pageActions(implicit site : Site) = Seq(
    ("view", controllers.routes.Asset.view(volumeId, fullSlot.slot.id, assetId), Permission.VIEW, true),
    ("edit", controllers.routes.Asset.edit(volumeId, containerId, assetId), Permission.EDIT, true),
    ("remove", controllers.routes.Asset.remove(volumeId, containerId, assetId), Permission.CONTRIBUTE, true)
  ).filter(a => a._4 == true && permission >= a._3).map(a => (a._1, a._2, a._3))
}

final class ContainerTimeseries private[models] (override val asset : Asset with TimeseriesData, container : Container, position_ : Option[Offset], name_ : String, body_ : Option[String]) extends ContainerAsset(asset, container, position_, name_, body_) {
  override def duration : Offset = asset.duration
  override def extent : Option[Range[Offset]] = position.map(start =>
    Range[Offset](start, start + duration)(PGSegment))
}

object ContainerAsset extends Table[ContainerAsset]("container_asset") {
  private[models] def make(asset : Asset, container : Container)(position : Option[Offset], name : String, body : Option[String]) = asset match {
    case ts : TimeseriesData => new ContainerTimeseries(ts, container, position, name, body)
    case _ => new ContainerAsset(asset, container, position, name, body)
  }
  private[models] val columns = Columns[
    Option[Offset], String,  Option[String]](
    'position,      'name,  'body)
  private[models] val containerColumns = columns.join(Asset.row, "container_asset.asset = asset.id")
  private[models] def containerRow(cont : Container) = containerColumns map {
    case (link ~ asset) => (make(asset, cont) _).tupled(link)
  }
  private[models] val row = containerColumns.join(Container.row, "container_asset.container = container.id") map {
    case (link ~ asset ~ cont) => (make(asset, cont) _).tupled(link)
  }

  /** Retrieve a specific asset link by asset id and container id.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  def get(asset : Asset.Id, container : Container.Id)(implicit site : Site) : Option[ContainerAsset] =
    row.SQL("WHERE container_asset.asset = {asset} AND container_asset.container = {cont} AND", Volume.condition).
      on('asset -> asset, 'cont -> container, 'identity -> site.identity.id, 'superuser -> site.superuser).singleOpt()
  /** Retrieve a specific asset link by asset.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  private[models] def get(asset : Asset)(implicit site : Site) : Option[ContainerAsset] = {
    val row = columns.join(Container.row, "container_asset.container = container.id") map {
      case (link ~ cont) => (make(asset, cont) _).tupled(link)
    }
    row.SQL("WHERE container_asset.asset = {asset} AND", Volume.condition).
      on('asset -> asset.id, 'identity -> site.identity.id, 'superuser -> site.superuser).singleOpt
  }
  /** Retrieve a specific asset link by container and asset id.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def get(container : Container, asset : Asset.Id)(implicit db : Site.DB) : Option[ContainerAsset] =
    containerRow(container).
      SQL("WHERE container_asset.container = {container} AND container_asset.asset = {asset}").
      on('container -> container.id, 'asset -> asset).singleOpt

  /** Retrieve the set of assets directly contained by a single container.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def getContainer(container : Container)(implicit db : Site.DB) : Seq[ContainerAsset] =
    containerRow(container).
      SQL("WHERE container_asset.container = {container} ORDER BY container_asset.position NULLS FIRST").
      on('container -> container.id).list

  /** Find the assets in a container with the given name. */
  def findName(container : Container, name : String)(implicit db : Site.DB) : Seq[ContainerAsset] =
    containerRow(container).
      SQL("WHERE container_asset.container = {container} AND container_asset.name = {name}").
      on('container -> container.id, 'name -> name).list

  /** Create a new link between an asset and a container.
    * This can change effective permissions on this asset, so care must be taken when using this function with existing assets. */
  def create(container : Container, asset : Asset, position : Option[Offset] = None, name : String, body : Option[String] = None)(implicit site : Site) : ContainerAsset = {
    Audit.add(table, SQLArgs('container -> container.id, 'asset -> asset.id, 'position -> position, 'name -> name, 'body -> body)).execute()
    new ContainerAsset(asset, container, position, name, body)
  }
}

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an ContainerAsset within the context of a Slot. */
sealed class SlotAsset protected (val link : ContainerAsset, val slot : Slot, excerpt_ : Option[Boolean] = None) extends SitePage with BackedAsset with InVolume {
  def slotId = slot.id
  def volume = link.volume
  def source = link.asset.source
  private var _excerpt = excerpt_
  /** Whether this clip has been vetted for public release, permissions permitting. */
  def excerpt : Boolean = _excerpt.getOrElse(false)
  /** Whether if this clip has been promoted for toplevel display. */
  def toplevel : Boolean = _excerpt.isDefined
  def position : Option[Offset] =
    (for { s <- slot.segment.lowerBound ; l <- link.position }
      yield ((l - s).max(0))).
      orElse(link.position)
  def duration : Offset =
    (for { s <- slot.segment.upperBound ; l <- link.position }
      yield ((s - l).min(link.duration))).
      getOrElse(link.duration)

  /** Update the given values in the database and this object in-place.
    * @param excerpt changes both toplevel (`None` or `Some(false)`) and excerpt (`Some(true)`)
    */
  def change(excerpt : Option[Boolean] = _excerpt)(implicit site : Site) : Unit = {
    if (excerpt != _excerpt)
    {
      val ids = SQLArgs('slot -> slotId, 'asset -> link.assetId)
      excerpt.fold {
        Audit.remove("toplevel_asset", ids).execute() : Unit
      } { e =>
        Audit.changeOrAdd("toplevel_asset", SQLArgs('excerpt -> e), ids)
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
  def permission(implicit site : Site) : Permission.Value =
    slot.dataPermission(classification)

  def pageName(implicit site : Site) = link.name
  def pageParent(implicit site : Site) = if(slot.container.top) { Some(slot.volume) } else { Some(slot) }
  def pageURL(implicit site : Site) = controllers.routes.Asset.view(volume.id, slotId, link.assetId)
  def pageActions(implicit site : Site) = Seq(
    ("view", controllers.routes.Asset.view(volumeId, slotId, link.assetId), Permission.VIEW, true),
    ("edit", controllers.routes.Asset.edit(volumeId, link.containerId, link.assetId), Permission.EDIT, true),
    ("remove", controllers.routes.Asset.remove(volumeId, link.containerId, link.assetId), Permission.CONTRIBUTE, slot.isFull)
  ).filter(a => a._4 == true && permission >= a._3).map(a => (a._1, a._2, a._3))
}

final class SlotTimeseries private[models] (override val link : ContainerTimeseries, slot : Slot, excerpt_ : Option[Boolean] = None) extends SlotAsset(link, slot, excerpt_) with TimeseriesData {
  override def source = link.asset.source
  def entire = link.asset.entire && link.position.fold(true) { l =>
    slot.segment.lowerBound.fold(true)(_ <= l) &&
    slot.segment.upperBound.fold(true)(_ >= l + link.asset.duration)
  }
  def segment = {
    val b = link.asset.segment
    val lb = b.lowerBound.get
    val ub = b.upperBound.get
    val lbn = (for { s <- slot.segment.lowerBound ; l <- link.position }
      yield (lb + (s - l).max(0))).
      getOrElse(lb)
    val ubn = (for { s <- slot.segment.lowerBound ; l <- link.position }
      yield (lbn + (s - l).min(ub - lbn))).
      getOrElse(ub)
    Range[Offset](lbn, ubn)(PGSegment)
  }
  override def duration : Offset = super[SlotAsset].duration
}

object SlotAsset {
  private def make(link : ContainerAsset, slot : Slot, excerpt : Option[Boolean]) = link match {
    case ts : ContainerTimeseries => new SlotTimeseries(ts, slot, excerpt)
    case _ => new SlotAsset(link, slot, excerpt)
  }
  private implicit val tableName : FromTable = FromTable("toplevel_asset")
  private val columns = Columns[
    Boolean](
    SelectColumn("excerpt"))
  private def condition(segment : String = "slot.segment") =
    "(container_asset.position IS NULL OR container_asset.position <@ " + segment + " OR segment_shift(segment(" + Asset.duration + "), container_asset.position) && " + segment + ")"
  private def volumeRow(vol : Volume) = ContainerAsset.containerColumns.
    join(Container.volumeRow(vol), "container_asset.container = container.id").
    join(Slot.columns, "container.id = slot.source").
    leftJoin(columns, "slot.id = toplevel_asset.slot AND asset.id = toplevel_asset.asset") map {
      case (link ~ asset ~ cont ~ slot ~ excerpt) => make((ContainerAsset.make(asset, cont) _).tupled(link), (Slot.make(cont) _).tupled(slot), excerpt)
    }

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume). */
  def get(asset : Asset.Id, slot : Slot.Id)(implicit site : Site) : Option[SlotAsset] = {
    val row = ContainerAsset.row.
      join(Slot.columns, "container.id = slot.source").
      leftJoin(columns, "slot.id = toplevel_asset.slot AND asset.id = toplevel_asset.asset") map {
      case (link ~ slot ~ excerpt) => make(link, (Slot.make(link.container) _).tupled(slot), excerpt)
    }
    row.SQL("WHERE slot.id = {slot} AND asset.id = {asset} AND", condition()).
      on('asset -> asset, 'slot -> slot, 'identity -> site.identity.id, 'superuser -> site.superuser).singleOpt
  }

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot)(implicit db : Site.DB) : Seq[SlotAsset] = {
    val row = ContainerAsset.containerRow(slot.container).
      leftJoin(columns, "asset.id = toplevel_asset.asset AND toplevel_asset.slot = {slot}") map {
      case (link ~ excerpt) => make(link, slot, excerpt)
    }
    row.SQL("WHERE container_asset.container = {container} AND", condition("{segment}"), "ORDER BY container_asset.position NULLS FIRST").
    on('slot -> slot.id, 'container -> slot.containerId, 'segment -> slot.segment).list
  }

  /** Build the SlotAsset for the given ContainerAsset#container.fullSlot. */
  private[models] def getFull(ca : ContainerAsset)(implicit db : Site.DB) : SlotAsset = {
    if (ca.container._fullSlot.isEmpty) {
      Slot.containerRow(ca.container).leftJoin(columns, "slot.id = toplevel_asset.slot AND toplevel_asset.asset = {asset}").
        map { case (slot ~ excerpt) =>
          ca.container._fullSlot() = slot
          make(ca, slot, excerpt)
        }.SQL("WHERE slot.source = {container} AND slot.segment = '(,)'").
        on('container -> ca.containerId, 'asset -> ca.assetId).single
    } else {
      val excerpt = columns.SQL("WHERE toplevel_asset.slot = {slot} AND toplevel_asset.asset = {asset}").
        on('slot -> ca.container.fullSlot.id, 'asset -> ca.assetId).singleOpt
      make(ca, ca.container.fullSlot, excerpt)
    }
  }

  /** Retrieve the list of all top-level assets. */
  private[models] def getToplevel(volume : Volume)(implicit db : Site.DB) : Seq[SlotAsset] =
    getSlot(volume.topSlot) ++
      volumeRow(volume).SQL("WHERE toplevel_asset.excerpt IS NOT NULL AND container.volume = {vol} AND", condition()).
      on('vol -> volume.id).list

  /** Find an asset suitable for use as a volume thumbnail.
    * TODO: check permissions, and find a better way to do this altogether */
  private[models] def getThumb(volume : Volume)(implicit db : Site.DB) : Option[SlotAsset] =
    volumeRow(volume).SQL("""
        WHERE (toplevel_asset.excerpt IS NOT NULL OR container.top AND slot.segment = '(,)')
          AND (format.id = {video} OR format.mimetype LIKE 'image/%')
          AND container.volume = {vol} AND""", condition(), "LIMIT 1").
      on('vol -> volume.id, 'video -> TimeseriesFormat.VIDEO).singleOpt
}
