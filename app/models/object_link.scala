package models

import java.sql.Date
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

final class ObjectLink private (val containerId : Container.Id, val objId : Object.Id, title_ : String, description_ : Option[String]) extends TableRow with SitePage {
  def id = (containerId, objId)
  private[this] var _title = title_
  def title = _title
  private[this] var _description = description_
  def description = _description

  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    Audit.SQLon(AuditAction.change, "object_link", "SET title = {title}, description = {description} WHERE container = {container} AND object = {obj}")('obj -> objId, 'container -> containerId, 'title -> title, 'description -> description).execute()(site.db)
    _title = title
    _description = description
  }

  private[ObjectLink] val _container = CachedVal[Container, Site](Container.get(containerId)(_).get)
  def container(implicit site : Site) : Container = _container
  private[ObjectLink] val _obj = CachedVal[Object, Site.DB](Object.get(objId)(_).get)
  def obj(implicit site : Site.DB) : Object = _obj

  /* object permissions depend on study permissions, but can be further restricted by consent levels */
  def permission(implicit site : Site) : Permission.Value = {
    val p = container.study.permission
    val c = obj(site.db).consent
    if (c > Consent.DEIDENTIFIED && (
      (c > Consent.SHARED && p < Permission.EDIT) 
      || site.access < Permission.DOWNLOAD))
      Permission.NONE
    else
      p
  }

  def pageName(implicit site : Site) = title
  def pageParent(implicit site : Site) = Some(container)
  def pageURL = controllers.routes.Object.view(containerId, objId).url

  def comments(implicit db : Site.DB) = Comment.getObjectLink(this)
  def addComment(text : String)(implicit site : Site) = Comment.create(this, text)
}

object ObjectLink extends TableView[ObjectLink]("object_link") {
  private[this] def make(containerId : Container.Id, objId : Object.Id, title : String, description : Option[String]) =
    new ObjectLink(containerId, objId, title, description)
  private[models] val row = Anorm.rowMap(make _, col("container"), col("object"), col("title"), col("description"))
  private[this] def rowContainer(container : Container) = row map { o => o._container() = container ; o }

  private[this] def get(c : Container.Id, o : Object.Id)(implicit db : Site.DB) : Option[ObjectLink] =
    SQL("SELECT " + * + " FROM object_link WHERE container = {container} AND object = {object}").
      on('container -> c, 'object -> o).singleOpt(row)
  private[models] def get(c : Container, o : Object.Id)(implicit db : Site.DB) : Option[ObjectLink] =
    SQL("SELECT " + * + " FROM object_link WHERE container = {container} AND object = {object}").
      on('container -> c.id, 'object -> o).singleOpt(rowContainer(c))

  private[models] def getObjects(c : Container)(implicit db : Site.DB) : Seq[ObjectLink] =
    SQL("SELECT " + * + " FROM object_link WHERE container = {container}").
      on('container -> c.id).list(rowContainer(c))
  private[models] def getContainers(o : Object)(implicit site : Site) : Seq[ObjectLink] =
    SQL("SELECT " + * + ", " + Container.* + " FROM object_link JOIN " + Container.src + " ON container.id = container WHERE object = {obj} AND " + Container.condition).
      on('obj -> o.id, 'identity -> site.identity.id).list((row ~ Container.row) map { case (l ~ c) =>
        l._container() = c
        l._obj() = o
        l
      })(site.db)
}
