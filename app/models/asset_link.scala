package models

import java.sql.Date
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

final class AssetLink private (val containerId : Container.Id, val assetId : Asset.Id, title_ : String, description_ : Option[String]) extends TableRow with SitePage with Annotated {
  def id = (containerId, assetId)
  private[this] var _title = title_
  def title = _title
  private[this] var _description = description_
  def description = _description

  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    Audit.change("asset_link", SQLArgs('title -> title, 'description -> description), SQLArgs('container -> containerId, 'asset -> assetId)).execute()(site.db)
    _title = title
    _description = description
  }

  private[AssetLink] val _container = CachedVal[Container, Site](Container.get(containerId)(_).get)
  def container(implicit site : Site) : Container = _container
  private[AssetLink] val _asset = CachedVal[Asset, Site.DB](Asset.get(assetId)(_).get)
  def asset(implicit site : Site.DB) : Asset = _asset

  /* asset permissions depend on study permissions, but can be further restricted by consent levels */
  def permission(implicit site : Site) : Permission.Value = {
    /* XXX: These permissions need to be reviewed carefully */
    val p = container.permission
    val c = asset(site.db).consent
    val t = asset(site.db).classification 
    val a = site.access
    // shared, identified data withheld from un-authorized users
    if (a < Permission.DOWNLOAD && p > Permission.VIEW &&
       ((t <= Classification.IDENTIFIED && c < Consent.PUBLIC) ||
        (t <= Classification.EXCERPT && c < Consent.EXCERPTS)))
      Permission.VIEW
    // study members get full access
    else if (p >= Permission.EDIT)
      p
    // private, identified data withheld from others
    else if (t < Classification.DEIDENTIFIED && c < Consent.SHARED && p > Permission.VIEW)
      Permission.VIEW
    // anyone with VIEW permission can download non-data
    else if (t > Classification.ANALYSIS && p == Permission.VIEW)
      Permission.DOWNLOAD
    else
      p
  }

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

  private[this] def get(c : Container.Id, a : Asset.Id)(implicit db : Site.DB) : Option[AssetLink] =
    SELECT("WHERE container = {container} AND asset = {asset}").
      on('container -> c, 'asset -> a).singleOpt()
  private[models] def get(c : Container, a : Asset.Id)(implicit db : Site.DB) : Option[AssetLink] =
    SELECT("WHERE container = {container} AND asset = {asset}").
      on('container -> c.id, 'asset -> a).singleOpt(rowContainer(c))

  private[models] def getAssets(c : Container)(implicit db : Site.DB) : Seq[AssetLink] =
    SELECT("WHERE container = {container}").
      on('container -> c.id).list(rowContainer(c))
  private[models] def getContainers(a : Asset, all : Boolean)(implicit site : Site) : Seq[AssetLink] =
    JOIN(Container, "ON container.id = container", 
        if (all) "JOIN asset_parents({file}, {segment}) ON asset = asset_parents WHERE" 
        else "WHERE asset = {asset} AND", 
      Container.condition).
      on('asset -> a.id, 'file -> a.fileId, 'segment -> a.fileSegment, 'identity -> site.identity.id).list((row ~ Container.row) map { case (l ~ c) =>
        l._container() = c
        if (l.assetId == a.id)
          l._asset() = a
        l
      })(site.db)

  def create(container : Container, asset : Asset, title : String, description : Option[String] = None)(implicit site : Site) : AssetLink = {
    Audit.add(table, SQLArgs('container -> container.id, 'asset -> asset.id, 'title -> title, 'description -> description)).execute()(site.db)
    val link = new AssetLink(container.id, asset.id, title, description)
    link._container() = container
    link._asset() = asset
    link
  }
}
