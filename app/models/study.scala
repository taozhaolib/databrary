package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

final class Study private (val id : Study.Id, title_ : String, description_ : Option[String], val permission : Permission.Value) extends TableRow {
  override def hashCode = id.unId
  def equals(a : Study) = a.id == id

  private[this] var _title = title_
  def title = _title
  private[this] var _description = description_
  def description = _description

  private def args =
    Anorm.Args('id -> id, 'title -> title, 'description -> description)

  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    Audit.SQLon(AuditAction.change, "study", "SET title = {title}, description = {description} WHERE id = {id}")(args : _*).execute()(site.db)
    _title = title
    _description = description
  }

  def entityAccess(p : Permission.Value = Permission.NONE)(implicit db : Site.DB) = StudyAccess.getEntities(id, p)
}

object Study extends TableViewId[Study]("study") {
  private[this] def make(id : Id, title : String, description : Option[String], access : Permission.Value) =
    new Study(id, title, description, access)
  private[models] val row = Anorm.rowMap(make _, "id", "title", "description", "permission")
  private[models] val permission = "study_access_check(id, {identity})"
  private[models] override val * = "*, " + permission + " AS permission"

  def get(i : Study.Id)(implicit site : Site) : Option[Study] =
    SQL("SELECT " + * + " FROM " + table + " WHERE id = {id} AND " + permission + " >= 'VIEW'").
      on('id -> i, 'identity -> site.identity.id).singleOpt(row)(site.db)
    
  def create(title : String)(implicit site : Site) : Study = {
    val args = Anorm.Args('title -> title)
    Audit.SQLon(AuditAction.add, "study", Anorm.insertArgs(args), "*")(args : _*).single(row)(site.db)
  }
}

final case class StudyAccess(studyId : Study.Id, entityId : Identity.Id, access : Permission.Value, inherit : Permission.Value) extends TableRow {
  private def id =
    Anorm.Args('study -> studyId, 'entity -> entityId)
  private def args = 
    id ++ Anorm.Args('access -> access, 'inherit -> inherit)

  def set(implicit site : Site) : Unit = {
    val args = this.args
    if (Audit.SQLon(AuditAction.change, "study_access", "SET access = {access}, inherit = {inherit} WHERE study = {study} AND entity = {entity}")(args : _*).executeUpdate()(site.db) == 0)
      Audit.SQLon(AuditAction.add, "study_access", Anorm.insertArgs(args))(args : _*).execute()(site.db)
  }
  def remove(implicit site : Site) : Unit =
    StudyAccess.delete(studyId, entityId)

  private val _study = CachedVal[Study, Site](Study.get(studyId)(_).orNull)
  def study(implicit site : Site) : Study = _study
  private val _entity = CachedVal[Identity, Site.DB](Identity.get(entityId)(_))
  def entity(implicit db : Site.DB) : Identity = _entity
}

object StudyAccess extends TableView[StudyAccess]("study_access") {
  private[models] val row = Anorm.rowMap(StudyAccess.apply _, "study", "entity", "access", "inherit")

  def get(s : Study.Id, e : Identity.Id)(implicit db : Site.DB) : Option[StudyAccess] =
    SQL("SELECT * FROM " + table + " WHERE study = {study} AND entity = {entity}").
      on('study -> s, 'entity -> e).singleOpt(row)

  private[models] def getEntities(s : Study.Id, p : Permission.Value = Permission.NONE)(implicit db : Site.DB) =
    SQL("SELECT * FROM " + table + " JOIN " + Identity.table + " ON (entity = id) WHERE study = {study} AND access >= {access} ORDER BY access DESC").
      on('study -> s, 'access -> p).list(row ~ Identity.row).
      map({ case (a ~ e) => a._entity() = e; a })
  private[models] def getStudies(e : Identity.Id, p : Permission.Value = Permission.NONE)(implicit site : Site) =
    SQL("SELECT " + Study.* + " FROM " + table + " JOIN " + Study.table + " ON (study = id) WHERE entity = {entity} AND access >= {access} AND " + Study.permission + " >= 'VIEW' ORDER BY access DESC").
      on('entity -> e, 'access -> p, 'identity -> site.identity.id).list(row ~ Study.row)(site.db).
      map({ case (a ~ s) => a._study() = s; a })

  def delete(s : Study.Id, e : Identity.Id)(implicit site : Site) =
    Audit.SQLon(AuditAction.remove, "study_access", "WHERE study = {study} AND entity = {entity}")('study -> s, 'entity -> e).
      execute()(site.db)

  def check(s : Study.Id, e : Identity.Id)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT study_access_check({study}, {entity})").
      on('study -> s, 'entity -> e).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)
}
