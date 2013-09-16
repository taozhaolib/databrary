package models

import java.sql.Date
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

/** An embedding or link (in the filesystem sense) of an asset within a container.
  * An asset link includes the asset and container, along with a name and description for that particular link.
  * Permissions are checked in msot cases, as indicated.
  */
sealed class ContainerAsset protected (val asset : Asset, val container : Container, offset_ : Option[Offset], name_ : String, body_ : Option[String]) extends TableRow with InVolume {
  def assetId = asset.id
  def containerId = container.id
  def id = (assetId, containerId)
  def volume = container.volume
  private[this] var _offset = offset_
  /** Start point of this asset within the container. */
  def offset : Option[Offset] = _offset
  private[this] var _name = name_
  /** Title or name of the asset as used in the container. */
  def name : String = _name
  private[this] var _body = body_
  /** Optional description of this asset. */
  def body : Option[String] = _body

  /** Update the given values in the database and this object in-place. */
  def change(offset : Option[Offset] = _offset, name : String = _name, body : Option[String] = _body)(implicit site : Site) : Unit = {
    if (offset == _offset && name == _name && body == _body)
      return
    Audit.change("container_asset", SQLArgs('offset -> offset, 'name -> name, 'body -> body), SQLArgs('container -> containerId, 'asset -> assetId)).execute()
    _name = name
    _body = body
  }

  def duration : Offset = 0
  /** Range of times that this asset covers, or None for "global/floating". */
  def extent : Option[Range[Offset]] = offset.map(Range.singleton[Offset](_)(PGSegment))
}

final class ContainerTimeseries private[models] (override val asset : Asset with TimeseriesData, container : Container, offset_ : Option[Offset], name_ : String, body_ : Option[String]) extends ContainerAsset(asset, container, offset_, name_, body_) {
  override def duration : Offset = asset.duration
  override def extent : Option[Range[Offset]] = offset.map(start =>
    Range[Offset](start, start + duration)(PGSegment))
}

object ContainerAsset extends Table[ContainerAsset]("container_asset") {
  private[models] def make(asset : Asset, container : Container)(offset : Option[Offset], name : String, body : Option[String]) = asset match {
    case ts : TimeseriesData => new ContainerTimeseries(ts, container, offset, name, body)
    case _ => new ContainerAsset(asset, container, offset, name, body)
  }
  private[models] val columns = Columns[
    Option[Offset], String,  Option[String]](
    'offset,        'name,  'body)
  private[models] def containerRow(cont : Container) = (Asset.row ~ columns) map {
    case (asset ~ link) => (make(asset, cont) _).tupled(link)
  }
  private[models] val containerSrc = "container_asset JOIN " + Asset.src + " ON container_asset.asset = asset.id"
  private[models] val row = (Asset.row ~ Container.row ~ columns) map {
    case (asset ~ cont ~ link) => (make(asset, cont) _).tupled(link)
  }
  private[models] override val src = containerSrc + " JOIN " + Container.src + " ON container_asset.container = container.id"

  /** Retrieve a specific asset link by asset id and container id.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  def get(asset : Asset.Id, container : Container.Id)(implicit site : Site) : Option[ContainerAsset] =
    SELECT("WHERE container_asset.asset = {asset} AND container_asset.container = {cont} AND", Volume.condition).
    on('asset -> asset, 'cont -> container, 'identity -> site.identity.id).singleOpt()
  /** Retrieve a specific asset link by asset.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  private[models] def get(asset : Asset)(implicit site : Site) : Option[ContainerAsset] = {
    val row = (Container.row ~ columns) map {
      case (cont ~ link) => (make(asset, cont) _).tupled(link)
    }
    SQL("SELECT " + row.select + " FROM container_asset JOIN " + Container.src + " ON container_asset.container = container.id WHERE container_asset.asset = {asset} AND " + Volume.condition).
      on('asset -> asset.id, 'identity -> site.identity.id).singleOpt(row)
  }
  /** Retrieve a specific asset link by container and asset id.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def get(container : Container, asset : Asset.Id)(implicit db : Site.DB) : Option[ContainerAsset] = {
    val row = containerRow(container)
    SQL("SELECT " + row.select + " FROM " + containerSrc + " WHERE container_asset.container = {container} AND container_asset.asset = {asset}").
      on('container -> container.id, 'asset -> asset).singleOpt(row)
  }

  /** Retrieve the set of assets directly contained by a single container.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def getContainer(container : Container)(implicit db : Site.DB) : Seq[ContainerAsset] = {
    val row = containerRow(container)
    SQL("SELECT " + row.select + " FROM " + containerSrc + " WHERE container_asset.container = {container}").
      on('container -> container.id).list(row)
  }

  /** Create a new link between an asset and a container.
    * This can change effective permissions on this asset, so care must be taken when using this function with existing assets. */
  def create(container : Container, asset : Asset, offset : Option[Offset] = None, name : String, body : Option[String] = None)(implicit site : Site) : ContainerAsset = {
    Audit.add(table, SQLArgs('container -> container.id, 'asset -> asset.id, 'offset -> offset, 'name -> name, 'body -> body)).execute()
    new ContainerAsset(asset, container, offset, name, body)
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
  def offset : Option[Offset] =
    (for { s <- slot.segment.lowerBound ; l <- link.offset }
      yield ((l - s).max(0))).
      orElse(link.offset)
  def duration : Offset =
    (for { s <- slot.segment.upperBound ; l <- link.offset }
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
        if (Audit.change("toplevel_asset", SQLArgs('excerpt -> e), ids).executeUpdate() == 0)
          Audit.add("toplevel_asset", SQLArgs('excerpt -> e) ++ ids).execute()
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
  def pageParent(implicit site : Site) = Some(slot)
  def pageURL = controllers.routes.Asset.view(slotId, link.assetId).url
}

final class SlotTimeseries private[models] (override val link : ContainerTimeseries, slot : Slot, excerpt_ : Option[Boolean] = None) extends SlotAsset(link, slot, excerpt_) with TimeseriesData {
  override def source = link.asset.source
  def entire = link.asset.entire && link.offset.fold(true) { l =>
    slot.segment.lowerBound.fold(true)(_ <= l) &&
    slot.segment.upperBound.fold(true)(_ >= l + link.asset.duration)
  }
  def segment = {
    val b = link.asset.segment
    val lb = b.lowerBound.get
    val ub = b.upperBound.get
    val lbn = (for { s <- slot.segment.lowerBound ; l <- link.offset }
      yield (lb + (s - l).max(0))).
      getOrElse(lb)
    val ubn = (for { s <- slot.segment.lowerBound ; l <- link.offset }
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
  private val columns = Columns[
    Option[Boolean]](
    SelectColumn("toplevel_asset", "excerpt"))
  private val condition = "(container_asset.offset IS NULL OR container_asset.offset <@ slot.segment OR segment_shift(segment(" + Asset.duration + "), container_asset.offset) && slot.segment)"

  /** Retrieve a single SlotAsset by asset id and slot id.
    * This checks permissions on the slot('s container's volume). */
  def get(asset : Asset.Id, slot : Slot.Id)(implicit db : Site.DB) : Option[SlotAsset] = {
    val row = ContainerAsset.row ~ Slot.columns ~ columns map {
      case (link ~ slot ~ excerpt) => make(link, (Slot.make(link.container) _).tupled(slot), excerpt)
    }
    SQL("SELECT " + row.select + " FROM " + ContainerAsset.src + " JOIN " + Slot.baseSrc + " ON container.id = slot.source LEFT JOIN toplevel_asset ON slot.id = toplevel_asset.slot AND asset.id = toplevel_asset.asset WHERE slot.id = {slot} AND asset.id = {asset} AND " + condition).
      on('asset -> asset, 'slot -> slot).singleOpt(row)
  }

  /** Retrieve the list of all assets within the given slot. */
  private[models] def getSlot(slot : Slot)(implicit db : Site.DB) : Seq[SlotAsset] = {
    val row = ContainerAsset.containerRow(slot.container) ~ columns map {
      case (link ~ excerpt) => make(link, slot, excerpt)
    }
    SQL("SELECT " + row.select + " FROM " + ContainerAsset.containerSrc + " JOIN slot ON container_asset.container = slot.source LEFT JOIN toplevel_asset ON slot.id = toplevel_asset.slot AND asset.id = toplevel_asset.asset WHERE slot.id = {slot} AND " + condition).
      on('slot -> slot.id).list(row)
  }

  /** Retrieve the list of all top-level assets. */
  private[models] def getToplevel(volume : Volume)(implicit db : Site.DB) : Seq[SlotAsset] = {
    val row = Asset.row ~ ContainerAsset.columns ~ Container.volumeRow(volume) ~ Slot.columns ~ columns map {
      case (asset ~ link ~ cont ~ slot ~ excerpt) => make((ContainerAsset.make(asset, cont) _).tupled(link), (Slot.make(cont) _).tupled(slot), excerpt)
    }
    SQL("SELECT " + row.select + " FROM " + ContainerAsset.containerSrc + " JOIN container ON container_asset.container = container.id JOIN " + Slot.baseSrc + " ON container.id = slot.source LEFT JOIN toplevel_asset ON slot.id = toplevel_asset.slot AND asset.id = toplevel_asset.asset WHERE container.volume = {vol} AND " + condition + " AND (toplevel_slot.slot IS NOT NULL OR toplevel_asset.asset IS NOT NULL)").
      on('vol -> volume.id).list(row)
  }
}
