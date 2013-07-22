package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

class Annotation protected (val id : Annotation.Id, val whoId : Identity.Id, val when : Timestamp, val containerId : Container.Id, val objId : Option[Object.Id]) extends TableRowId[Annotation] {
  protected[models] val _who = CachedVal[Identity, Site.DB](Identity.get(whoId)(_).get)
  def who(implicit db : Site.DB) : Identity = _who
  protected[models] val _container = CachedVal[Container, Site](Container.get(containerId)(_).get)
  def container(implicit site : Site) : Container = _container
  protected[models] val _obj = CachedVal[Option[ObjectLink], Site](s => objId.flatMap(o => container(s).getObject(o)(s.db)))
  def obj(implicit site : Site) : Option[ObjectLink] = _obj
  def target(implicit site : Site) : SitePage = if (objId.isEmpty) container else obj.get
}

final class Comment private (override val id : Comment.Id, whoId : Identity.Id, when : Timestamp, containerId : Container.Id, objId : Option[Object.Id], val text : String) extends Annotation(id, whoId, when, containerId, objId) with TableRowId[Comment] {
}

private[models] sealed abstract class AnnotationView[R <: Annotation](table : String) extends TableView[R](table) with HasId[R] {
  private[models] def get(id : Id)(implicit db : Site.DB) : Option[R] =
    SQL("SELECT " + * + " FROM " + table + " WHERE id = {id}").
      on('id -> id).singleOpt(row)

  private[this] def getList(container : Container.Id, obj : Option[Option[Object.Id]])(implicit db : Site.DB) : Seq[R] = obj.fold {
      SQL("SELECT " + * + " FROM " + table + " WHERE container = {container} ORDER BY id DESC").on('container -> container)
    } { _.fold {
      SQL("SELECT " + * + " FROM " + table + " WHERE container = {container} AND object IS NULL ORDER BY id DESC").on('container -> container)
    } { obj =>
      SQL("SELECT " + * + " FROM " + table + " WHERE container = {container} AND object = {obj} ORDER BY id DESC").on('container -> container, 'obj -> obj)
    } }.list(row)

  private[models] def getContainer(container : Container, only : Boolean = false)(implicit db : Site.DB) : Seq[R] =
    getList(container.id, if (only) Some(None) else None).map({ c =>
      c._container() = container
      c
    })
  private[models] def getObjectLink(obj : ObjectLink)(implicit db : Site.DB) : Seq[R] =
    getList(obj.containerId, Some(Some(obj.objId))).map({ c =>
      c._obj() = Some(obj)
      c
    })
  private[models] def getEntity(i : Identity)(implicit site : Site) : Seq[R] =
    SQL("SELECT " + * + ", " + Container.* + " FROM " + table + " JOIN " + Container.src + " ON " + col("id") + " = container.id WHERE " + col("who") + " = {who} AND " + Container.condition + " ORDER BY " + col("id") + " DESC").
      on('who -> i.id, 'identity -> site.identity.id).list(row map { c =>
        c._who() = i
        c
      })(site.db)
}

private[models] object Annotation extends AnnotationView[Annotation]("annotation") {
  private[this] def make(id : Annotation.Id, whoId : Identity.Id, when : Timestamp, containerId : Container.Id, objId : Option[Object.Id]) =
    new Annotation(id, whoId, when, containerId, objId)
  private[models] val row = Anorm.rowMap(make _, col("id"), col("who"), col("when"), col("container"), col("object"))
}

object Comment extends AnnotationView[Comment]("comment") {
  private[this] def make(id : Comment.Id, whoId : Identity.Id, when : Timestamp, containerId : Container.Id, objId : Option[Object.Id], text : String) =
    new Comment(id, whoId, when, containerId, objId, text)
  private[models] val row = Anorm.rowMap(make _, col("id"), col("who"), col("when"), col("container"), col("object"), col("text"))

  private[this] def create(container : Container.Id, obj : Option[Object.Id], text : String)(implicit site : Site) = {
    val args = Anorm.Args('who -> site.identity.id, 'container -> container, 'object -> obj, 'text -> text)
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
  private[models] def create(obj : ObjectLink, text : String)(implicit site : Site) : Comment = {
    val c = create(obj.containerId, Some(obj.objId), text)
    c._obj() = Some(obj)
    c
  }
}

trait CommentPage extends SitePage {
  def comments(only : Boolean = false)(implicit db : Site.DB) : Seq[Comment]
  def addComment(text : String)(implicit site : Site) : Comment
}
