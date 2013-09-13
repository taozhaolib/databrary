package models

import java.sql.Date
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

/** A particular asset occupying a specific position within a context. */
private[models] abstract trait PositionedAsset {
  def asset : Asset
  def assetId : Asset.Id
  /** Start point of this asset with respect to the start of this context, or None for "global/floating". */
  def offset : Option[Offset] = extent.lowerBound
  protected def timeseries : Option[TimeseriesData] = cast[TimeseriesData](Asset)
  protected def duration : Offset = timeseries.fold(0)(_.duration)
  /** Range of times that this asset covers within this context, or None for "global/floating". */
  def extent : Option[Range[Offset]] = offset map { start =>
    timeseries.fold(Range[Offset].singleton(start))(ts => Range[Offset](start, start + ts._duration))
  }
}

/** An embedding or link (in the filesystem sense) of an asset within a container.
  * An asset link includes the asset and container, along with a name and description for that particular link.
  * Permissions are checked in msot cases, as indicated.
  */
sealed class AssetContainer protected (override val asset : Asset, val container : Container, offset_ : Option[Offset], name_ : String, body_ : Option[String]) extends TableRow with SitePage with PositionedAsset {
  def assetId = asset.id
  def containerId = container.id
  def id = (assetId, containerId)
  private[this] var _offset = offset_
  /** Start point of this asset within the container. */
  override def offset : Option[Offset] = _offset
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
    Audit.change("asset_container", SQLArgs('offset -> offset, 'name -> name, 'body -> body), SQLArgs('container -> containerId, 'asset -> assetId)).execute()
    _name = name
    _body = body
  }

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = Some(container)
  def pageURL = controllers.routes.Asset.view(containerId, assetId).url
}

final class TimeseriesContainer private[models] (override val asset : Asset with TimeseriesData, container : Container, offset : Option[Offset], name : String, body : Option[String]) extends AssetContainer(asset, container, offset, name, body)

object AssetContainer extends Table[AssetContainer]("asset_container") {
  private def make(asset : Asset, container : Container)(offset : Option[Offset], name : String, body : Option[String]) = asset match {
    case ts : TimeseriesData => new TimeseriesContainer(ts, container, offset, name, body)
    case _ => new AssetContainer(asset, container, offset, name, body)
  }
  private[models] val columns = Columns[
    Option[Offset], String,  Option[String]](
    'offset,        'name,  'body)
  private[models] def containerRow(cont : Container) = (Asset.row ~ columns) map {
    case (asset ~ link) => (make(asset, cont) _).tupled(link)
  }
  private[models] val containerSrc = "asset_container JOIN " + Asset.src + " ON asset_container.asset = asset.id"
  private[models] val row = (Asset.row ~ Container.row ~ columns) map {
    case (asset ~ cont ~ link) => (make(asset, cont) _).tupled(link)
  }
  private[models] override val src = containerSrc + " JOIN " + Container.src + " ON asset_container.container = container.id"

  /** Retrieve a specific asset link by asset id.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  private[models] def get(asset : Asset.Id)(implicit site : Site) : Option[AssetContainer] =
    SELECT("WHERE asset_container.asset = {asset} AND", Volume.condition).
    on('asset -> asset, 'identity -> site.identity.id).singleOpt()
  /** Retrieve a specific asset link by asset.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  private[models] def get(asset : Asset)(implicit site : Site) : Option[AssetContainer] = {
    val row = (Container.row ~ columns) map {
      case (cont ~ link) => (make(asset, cont) _).tupled(link)
    }
    SQL("SELECT " + row.select + " FROM asset_container JOIN " + Container.src + " ON asset_container.container = container.id WHERE asset_container.asset = {asset} AND", Volume.condition).
      on('asset -> asset.id, 'identity -> site.identity.id).singleOpt(row)
  }
  /** Retrieve a specific asset link by container and asset id.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def get(container : Container, asset : Asset.Id)(implicit db : Site.DB) : Option[AssetContainer] = {
    val row = containerRow(container)
    SQL("SELECT " + row.select + " FROM " + containerSrc + " WHERE asset_container.container = {container} AND asset_container.asset = {asset}").
      on('container -> container.id, 'asset -> asset).singleOpt(row)
  }

  /** Retrieve the set of assets directly contained by a single container.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def getContainer(container : Container)(implicit db : Site.DB) : Seq[AssetContainer] = {
    val row = containerRow(container)
    SQL("SELECT " + row.select + " FROM " + containerSrc + " WHERE asset_container.container = {container}").
      on('container -> container.id).list(row)
  }

  /** Create a new link between an asset and a container.
    * This can change effective permissions on this asset, so care must be taken when using this function with existing assets. */
  def create(container : Container, asset : Asset, offset : Option[Offset] = None, name : String, body : Option[String] = None)(implicit site : Site) : AssetContainer = {
    Audit.add(table, SQLArgs('container -> container.id, 'asset -> asset.id, 'offset -> offset, 'name -> name, 'body -> body)).execute()
    new AssetContainer(asset, container, offset, name, body)
  }
}

/** A segment of an asset as used in a slot.
  * This is a "virtual" model representing an AssetContainer within the context of a Slot. */
sealed class SlotAsset protected (val link : AssetContainer, val slot : Slot) extends PositionedAsset with BackedAsset {
  def asset = link.asset
  def assetId = link.assetId
  def source = link.asset.source
  def sourceId = link.asset.sourceId
  override def offset : Option[Offset] =
    slot.lowerBound.flatMap(l => link.offset.map(_ - l)).
      fold(link.offset)(_.max(0))
  override protected def duration : Offset =
    slot.upperBound.flatMap(u => link.offset.map(u - _)).
      fold(super.duration)(_.min(super.duration))

  /** Effective permission the site user has over this segment, specifically in regards to the asset itself.
    * Asset permissions depend on volume permissions, but can be further restricted by consent levels. */
  def permission(implicit site : Site) : Permission.Value =
    slot.dataPermission(link.asset.classification)
}

case class SlotTimeseries private[models] (override val link : TimeseriesContainer, val slot : Slot) extends SlotAsset(link, slot) with TimeseriesData {
  def segment = {
    val b = link.asset.segment
    val lb = b.lowerBound.get
    val lbn = slot.lowerBound.flatMap(l => link.offset.map(l - _)).fold(lb)(lb + _.max(0)),
    val ub = b.upperBound.get
    val ubn = slot.upperBound.flatMap(u => link.offset.map(u - _)).fold(ub)(lbn + _.min(ub - lbn))
    Range[Offset](lbn, ubn)
  }
}

object AssetSegment {
  private def make(link : AssetContainer, slot : Slot) = link match {
    case ts : TimeseriesContainer => new SlotTimeseries(ts, slot)
    case _ => new SlotAsset(link, slot)
  }
  private[models] def getSlot(slot : Slot)(implicit db : Site.DB) : Seq[AssetSegment] = {
    val row = AssetContainer.containerRow(container).map(make(_, slot))
    SQL("SELECT " + row.select + " FROM " + containerSrc + " WHERE asset_container.container = {container} AND (asset_container.offset IS NULL OR asset_container.offset <@ slot.segment OR segment_shift(segment(" + Asset.duration + "), asset_container.offset) && slot.segment)").
      on('container -> slot.container.id).list(row)
  }
}
