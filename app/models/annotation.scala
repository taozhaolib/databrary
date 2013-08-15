package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

class Annotation protected (val id : Annotation.Id, val whoId : Party.Id, val when : Timestamp, val containerId : Container.Id, val assetId : Option[Asset.Id]) extends TableRowId[Annotation] {
  protected[models] val _who = CachedVal[Party, Site](Party.get(whoId)(_).get)
  def who(implicit site : Site) : Party = _who
  protected[models] val _container = CachedVal[Container, Site](Container.get(containerId)(_).get)
  def container(implicit site : Site) : Container = _container
  protected[models] val _asset = CachedVal[Option[AssetLink], Site](s => assetId.flatMap(o => container(s).getAsset(o)(s.db)))
  def asset(implicit site : Site) : Option[AssetLink] = _asset
  def target(implicit site : Site) : SitePage = if (assetId.isEmpty) container else asset.get
}

final class Comment private (override val id : Comment.Id, whoId : Party.Id, when : Timestamp, containerId : Container.Id, assetId : Option[Asset.Id], val text : String) extends Annotation(id, whoId, when, containerId, assetId) with TableRowId[Comment] {
}

private[models] sealed abstract class AnnotationView[R <: Annotation with TableRowId[R]](table : String) extends TableId[R](table) {
  private[models] def get(id : Id)(implicit db : Site.DB) : Option[R] =
    SELECT("WHERE id = {id}").
      on('id -> id).singleOpt(row)

  private[models] def getContainer(container : Container, only : Boolean = false)(implicit db : Site.DB) : Seq[R] =
    SQL("SELECT " + * + (
      if (only)
       " FROM " + table + " WHERE asset IS NULL AND"
      else container match {
        case _ : Slot  => " FROM " + table + " WHERE"
        case _ : Study => ", " + Slot.columns.select + " FROM " + table + " LEFT JOIN slot ON container = slot.id WHERE slot.study = {container} OR"
      }) + " container = {container} ORDER BY " + table + ".id DESC").
      on('container -> container.id).list((row ~ Slot.columns.?) map { case (a ~ s) =>
        // For some reason this ends up casting Slot as Study?
        // a._container() = s.fold(container)(
        a._container() = if (a.containerId == container.id) container 
          else Slot.baseMake(container.asInstanceOf[Study])(s.get)
        a
      })
  private[models] def getAssetLink(asset : AssetLink)(implicit db : Site.DB) : Seq[R] =
    SELECT("WHERE container = {container} AND asset = {asset} ORDER BY id DESC").
      on('container -> asset.containerId, 'asset -> asset.assetId).list(row map { a =>
        a._asset() = Some(asset)
        a
      })
  private[models] def getParty(i : Party)(implicit site : Site) : Seq[R] =
    JOIN(Container, "ON " + table + ".container = container.id WHERE " + table + ".who = {who} AND " + Container.condition + " ORDER BY " + table + ".id DESC").
      on('who -> i.id, 'identity -> site.identity.id).list(row map { a =>
        a._who() = i
        a
      })(site.db)
}

private[models] object Annotation extends AnnotationView[Annotation]("annotation") {
  private[models] val row = Columns[
    Id,  Party.Id, Timestamp, Container.Id, Option[Asset.Id]](
    'id, 'who,     'when,     'container,   'asset).map {
    (id, whoId, when, containerId, assetId) => new Annotation(id, whoId, when, containerId, assetId)
  }
}

object Comment extends AnnotationView[Comment]("comment") {
  private[models] val row = Columns[
    Id,  Party.Id, Timestamp, Container.Id, Option[Asset.Id], String](
    'id, 'who,     'when,     'container,   'asset,           'text).map {
    (id, whoId, when, containerId, assetId, text) => new Comment(id, whoId, when, containerId, assetId, text)
  }

  private[this] def create(container : Container.Id, asset : Option[Asset.Id], text : String)(implicit site : Site) = {
    val args = Anorm.Args('who -> site.identity.id, 'container -> container, 'asset -> asset, 'text -> text)
    val c = SQL("INSERT INTO " + table + " " + Anorm.insertArgs(args) + " RETURNING " + *).
      on(args : _*).single(row)(site.db)
    c._who() = site.identity
    c
  }
  private[models] def create(container : Container, text : String)(implicit site : Site) : Comment = {
    val c = create(container.id, None, text)
    c._container() = container
    c
  }
  private[models] def create(asset : AssetLink, text : String)(implicit site : Site) : Comment = {
    val c = create(asset.containerId, Some(asset.assetId), text)
    c._asset() = Some(asset)
    c
  }
}

trait CommentPage extends SitePage {
  def comments(only : Boolean = false)(implicit db : Site.DB) : Seq[Comment]
  def addComment(text : String)(implicit site : Site) : Comment
}
