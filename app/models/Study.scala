package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple.Session
import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

final class Study private (val id : Int, title_ : String, description_ : Option[String], val permission : Permission.Value) extends TableRow {
  override def hashCode = id
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
    implicit val db = site.db.conn
    Audit.SQLon(AuditAction.change, "study", "SET title = {title}, description = {description} WHERE id = {id}")(args : _*).execute()
    _title = title
    _description = description
  }

  def entityAccess(p : Permission.Value = Permission.NONE)(implicit db : Session) = StudyAccess.getEntities(id, p)
}

object Study extends TableViewId[Study]("study", _.id) {
  private[this] def apply(id : Int, title : String, description : Option[String], access : Permission.Value) =
    new Study(id, title, description, access)
  private[models] val row = Anorm.rowMap(apply _, "id", "title", "description", "permission")
  private[models] val permission = "study_access_check(id, {identity})"
  private[models] override val * = "*, " + permission + " AS permission"

  def get(i : Int)(implicit site : Site) : Option[Study] =
    SQL("SELECT " + * + " FROM " + table + " WHERE id = {id} AND " + permission + " >= 'VIEW'").
      on('id -> i, 'identity -> site.identity).singleOpt(row)(site.db.conn)
    
  def create(title : String)(implicit site : Site) : Study = {
    implicit val db = site.db.conn
    val args = Anorm.Args('title -> title)
    Audit.SQLon(AuditAction.add, "study", Anorm.insertArgs(args), "*")(args : _*).single(row)
  }
}

final case class StudyAccess(studyId : Int, entityId : Int, access : Permission.Value, inherit : Permission.Value) extends TableRow {
  private def id =
    Anorm.Args('study -> studyId, 'entity -> entityId)
  private def args = 
    id ++ Anorm.Args('access -> access, 'inherit -> inherit)

  def set(implicit site : Site) : Unit = {
    implicit val db = site.db.conn
    val args = this.args
    if (Audit.SQLon(AuditAction.change, "study_access", "SET access = {access}, inherit = {inherit} WHERE study = {study} AND entity = {entity}")(args : _*).executeUpdate() == 0)
      Audit.SQLon(AuditAction.add, "study_access", Anorm.insertArgs(args))(args : _*).execute()
  }
  def remove(implicit site : Site) : Unit =
    StudyAccess.delete(studyId, entityId)

  private val _study = CachedVal[Study, Site](Study.get(studyId)(_).orNull)
  def study(implicit site : Site) : Study = _study
  private val _entity = CachedVal[Identity, Session](Identity.get(entityId)(_))
  def entity(implicit db : Session) : Identity = _entity
}

object StudyAccess extends TableView("study_access") {
  private[this] val row = Anorm.rowMap(StudyAccess.apply _, "study", "entity", "access", "inherit")

  def get(s : Int, e : Int)(implicit db : Session) : Option[StudyAccess] =
    SQL("SELECT * FROM " + table + " WHERE study = {study} AND entity = {entity}").
      on('study -> s, 'entity -> e).singleOpt(row)(db.conn)

  private[models] def getEntities(s : Int, p : Permission.Value = Permission.NONE)(implicit db : Session) =
    SQL("SELECT * FROM " + table + " JOIN " + Identity.table + " ON (entity = id) WHERE study = {study} AND access >= {access} ORDER BY access DESC").
      on('study -> s, 'access -> p).list(row ~ Identity.row)(db.conn).
      map({ case (a ~ e) => a._entity() = e; a })
  private[models] def getStudies(e : Int, p : Permission.Value = Permission.NONE)(implicit site : Site) =
    SQL("SELECT " + Study.* + " FROM " + table + " JOIN " + Study.table + " ON (study = id) WHERE entity = {entity} AND access >= {access} AND " + Study.permission + " >= 'VIEW' ORDER BY access DESC").
      on('entity -> e, 'access -> p, 'identity -> site.identity).list(row ~ Study.row)(site.db.conn).
      map({ case (a ~ s) => a._study() = s; a })

  private def delete(s : Int, e : Int)(implicit site : Site) =
    Audit.SQLon(AuditAction.remove, "study_access", "WHERE study = {study} AND entity = {entity}")('study -> s, 'entity -> e).
      execute()(site.db.conn)

  /*
  def check(s : Int, e : Int)(implicit db : Session) : Permission.Value =
    SQL("SELECT study_access_check({study}, {entity})").
      on('study -> s, 'entity -> e).single(scalar[Option[Permission.Value]])(db.conn).
      getOrElse(Permission.NONE)
  */
}
