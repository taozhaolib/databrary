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
sealed class ContainerAsset protected (val asset : Asset, val container : Container, position_ : Option[Offset], name_ : String, body_ : Option[String]) extends TableRow with InVolume {
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
  def change(position : Option[Offset] = _position, name : String = _name, body : Option[String] = _body) : Future[Boolean] = {
    if (position == _position && name == _name && body == _body)
      return Async(true)
    Audit.change("container_asset", SQLTerms('position -> position, 'name -> name, 'body -> body), SQLTerms('container -> containerId, 'asset -> assetId)).execute
      .andThen { case scala.util.Success(true) =>
        _position = position
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
}

final class ContainerTimeseries private[models] (override val asset : Asset with TimeseriesData, container : Container, position_ : Option[Offset], name_ : String, body_ : Option[String]) extends ContainerAsset(asset, container, position_, name_, body_) {
  override def duration : Offset = asset.duration
  override def extent : Option[Range[Offset]] = position.map(start =>
    Range[Offset](start, start + duration))
}

object ContainerAsset extends Table[ContainerAsset]("container_asset") {
  private val columns = Columns(
      SelectColumn[Option[Offset]]("position")
    , SelectColumn[String]("name")
    , SelectColumn[Option[String]]("body")
    ).map { (position, name, body) =>
      (asset : Asset, container : Container) => asset match {
        case ts : TimeseriesData => new ContainerTimeseries(ts, container, position, name, body)
        case a : Asset => new ContainerAsset(asset, container, position, name, body)
      }
    }
  private[models] val containerColumns =
    columns.join(Asset.row, "container_asset.asset = asset.id")
    .map { case (link, asset) => (cont : Container) => link(asset, cont) }
  private[models] def containerRow(cont : Container) =
    containerColumns.map(_(cont))
  private[models] def row(implicit site : Site) =
    containerColumns.join(Container.row(false), "container_asset.container = container.id") map {
      case (link, cont) => link(cont)
    }

  /** Retrieve a specific asset link by asset id and container id.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  def get(asset : Asset.Id, container : Container.Id)(implicit site : Site) : Future[Option[ContainerAsset]] =
    row.SELECT("WHERE container_asset.asset = ? AND container_asset.container = ? AND", Volume.condition)
      .apply(SQLArgs(asset, container) ++ Volume.conditionArgs).singleOpt

  /** Retrieve a specific asset link by asset.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access on the container. */
  private[models] def get(asset : Asset)(implicit site : Site) : Future[Option[ContainerAsset]] =
    columns.join(Container.row(false), "container_asset.container = container.id").map {
        case (link, cont) => link(asset, cont)
      }
      .SELECT("WHERE container_asset.asset = ? AND", Volume.condition)
      .apply(asset.id +: Volume.conditionArgs).singleOpt

  /** Retrieve a specific asset link by container and asset id.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  def get(container : Container, asset : Asset.Id) : Future[Option[ContainerAsset]] =
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
