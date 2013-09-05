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
final class AssetLink private (val containerId : Container.Id, val assetId : Asset.Id, title_ : String, description_ : Option[String]) extends TableRow with SitePage with Annotated {
  def id = (containerId, assetId)
  private[this] var _title = title_
  /** Title or name of the asset as used in the container. */
  def title = _title
  private[this] var _description = description_
  /** Optional description of this asset. */
  def description = _description

  /** Update the given values in the database and this object in-place. */
  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    Audit.change("asset_link", SQLArgs('title -> title, 'description -> description), SQLArgs('container -> containerId, 'asset -> assetId)).execute()(site.db)
    _title = title
    _description = description
  }

  /** The cached container object.  In most cases this will be set during retrieval, such that appropriate container permissions have already been checked at that time. */
  private[AssetLink] val _container = CachedVal[Container, Site](Container.get(containerId)(_).get)
  /** The container object for this link.
    * If this link associated with an unreadable container, this will throw an exception. */
  def container(implicit site : Site) : Container = _container
  private[AssetLink] val _asset = CachedVal[Asset, Site.DB](Asset.get(assetId)(_).get)
  /** The asset object for this link. */
  def asset(implicit db : Site.DB) : Asset = _asset

  /** Effective permission the site user has over this link, specifically in regards to the asset itself.
    * Asset permissions depend on study permissions, but can be further restricted by consent levels. */
  def permission(implicit site : Site) : Permission.Value =
    Permission.data(container.permission, asset(site.db).consent, asset(site.db).classification)

  def pageName(implicit site : Site) = title
  def pageParent(implicit site : Site) = Some(container)
  def pageURL = controllers.routes.Asset.view(containerId, assetId).url

  /* Annotated is proxy for linked asset */
  private[models] final def annotatedLevel = "asset"
  private[models] final def annotatedId = assetId
}

object AssetLink extends Table[AssetLink]("asset_link") {
  private[this] def make(containerId : Container.Id, assetId : Asset.Id, title : String, description : Option[String]) =
    new AssetLink(containerId, assetId, title, description)
  private[models] val row = Columns[
    Container.Id, Asset.Id, String,  Option[String]](
    'container,   'asset,   'title,  'description).
    map(make _)
  private[this] def rowContainer(container : Container) = row map { a => a._container() = container ; a }

  /** Retrieve a specific asset link by contanier and asset ids.
    * This does not check permissions on the container, so is unsafe. */
  private[this] def get(container : Container.Id, asset : Asset.Id)(implicit db : Site.DB) : Option[AssetLink] =
    SELECT("WHERE container = {container} AND asset = {asset}").
      on('container -> container, 'asset -> asset).singleOpt()
  /** Retrieve a specific asset link by container and asset id.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def get(container : Container, asset : Asset.Id)(implicit db : Site.DB) : Option[AssetLink] =
    SELECT("WHERE container = {container} AND asset = {asset}").
      on('container -> container.id, 'asset -> asset).singleOpt(rowContainer(container))

  /** Retrieve the set of assets directly contained by a single container.
    * This assumes that permissions have already been checked as the caller must already have the container. */
  private[models] def getAssets(container : Container)(implicit db : Site.DB) : Seq[AssetLink] =
    SELECT("WHERE container = {container}").
      on('container -> container.id).list(rowContainer(container))
  /** Retrieve the set of (viewable) containers containing this asset.
    * This checks for view permissions on containers, so may return an empty list, in which case the asset (link) itself should not be accessible.
    * @param all include all links of assets which contain this asset (e.g., parent timeseries of clips)
    */
  private[models] def getContainers(asset : Asset, all : Boolean)(implicit site : Site) : Seq[AssetLink] =
    JOIN(Container, "ON container.id = container", 
        if (all) "JOIN asset_parents({file}, {segment}) ON asset = asset_parents WHERE" 
        else "WHERE asset = {asset} AND", 
      Container.condition).
      on('asset -> asset.id, 'file -> asset.sourceId, 'segment -> cast[Clip](asset).map(_.segment), 'identity -> site.identity.id).list((row ~ Container.row) map { case (l ~ c) =>
        l._container() = c
        if (l.assetId == asset.id)
          l._asset() = asset
        l
      })(site.db)

  /** Create a new link between an asset and a container.
    * This can change effective permissions on this asset, so care must be taken when using this function with existing assets. */
  def create(container : Container, asset : Asset, title : String, description : Option[String] = None)(implicit site : Site) : AssetLink = {
    Audit.add(table, SQLArgs('container -> container.id, 'asset -> asset.id, 'title -> title, 'description -> description)).execute()(site.db)
    val link = new AssetLink(container.id, asset.id, title, description)
    link._container() = container
    link._asset() = asset
    link
  }
}
