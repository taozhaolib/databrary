package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

class Annotation protected (val id : Annotation.Id, val whoId : Entity.Id, val when : Timestamp, val containerId : Container.Id, val objId : Option[Object.Id]) extends TableRowId[Annotation] {
  protected[models] val _who = CachedVal[Entity, Site](Entity.get(whoId)(_).get)
  def who(implicit site : Site) : Entity = _who
  protected[models] val _container = CachedVal[Container, Site](Container.get(containerId)(_).get)
  def container(implicit site : Site) : Container = _container
  protected[models] val _obj = CachedVal[Option[ObjectLink], Site](s => objId.flatMap(o => container(s).getObject(o)(s.db)))
  def obj(implicit site : Site) : Option[ObjectLink] = _obj
  def target(implicit site : Site) : SitePage = if (objId.isEmpty) container else obj.get
}

final class Comment private (override val id : Comment.Id, whoId : Entity.Id, when : Timestamp, containerId : Container.Id, objId : Option[Object.Id], val text : String) extends Annotation(id, whoId, when, containerId, objId) with TableRowId[Comment] {
}

private[models] sealed abstract trait AnnotationView[R <: Annotation with TableRowId[R]] extends TableView[R] with HasId[R] {
  private[models] val row : RowParser[R]
  private[models] def get(id : Id)(implicit db : Site.DB) : Option[R] =
    SELECT("WHERE id = {id}").
      on('id -> id).singleOpt(row)

  private[models] def getContainer(container : Container, only : Boolean = false)(implicit db : Site.DB) : Seq[R] =
    SQL("SELECT " + * + (
      if (only)
       " FROM " + table + " WHERE object IS NULL AND"
      else container match {
        case _ : Slot  => " FROM " + table + " WHERE"
        case _ : Study => ", " + Slot.* + " FROM " + table + " LEFT JOIN slot ON container = slot.id WHERE slot.study = {container} OR"
      }) + " container = {container} ORDER BY " + col("id") + " DESC").
      on('container -> container.id).list((row ~ Slot.columns.?) map { case (a ~ s) =>
        // For some reason this ends up casting Slot as Study?
        // a._container() = s.fold(container)(
        a._container() = if (a.containerId == container.id) container 
          else Slot.baseMake(container.asInstanceOf[Study])(s.get)
        a
      })
  private[models] def getObjectLink(obj : ObjectLink)(implicit db : Site.DB) : Seq[R] =
    SELECT("WHERE container = {container} AND object = {obj} ORDER BY id DESC").
      on('container -> obj.containerId, 'obj -> obj.objId).list(row map { a =>
        a._obj() = Some(obj)
        a
      })
  private[models] def getEntity(i : Entity)(implicit site : Site) : Seq[R] =
    JOIN(Container, "ON " + col("container") + " = container.id WHERE " + col("who") + " = {who} AND " + Container.condition + " ORDER BY " + col("id") + " DESC").
      on('who -> i.id, 'identity -> site.identity.id).list(row map { a =>
        a._who() = i
        a
      })(site.db)
}

private[models] object Annotation extends TableColumnsId4[
    Annotation,   Entity.Id, Timestamp, Container.Id, Option[Object.Id]](
    "annotation", "who",     "when",    "container",  "object") with AnnotationView[Annotation] {
  private[this] def make(id : Id, whoId : Entity.Id, when : Timestamp, containerId : Container.Id, objId : Option[Object.Id]) =
    new Annotation(id, whoId, when, containerId, objId)
  private[models] val row = columns.map(make _)
}

object Comment extends TableColumnsId5[
    Comment,   Entity.Id, Timestamp, Container.Id, Option[Object.Id], String](
    "comment", "who",     "when",    "container",  "object",          "text") with AnnotationView[Comment] {
  private[this] def make(id : Id, whoId : Entity.Id, when : Timestamp, containerId : Container.Id, objId : Option[Object.Id], text : String) =
    new Comment(id, whoId, when, containerId, objId, text)
  private[models] val row = columns.map(make _)

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
