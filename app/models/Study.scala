package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple._
import java.sql.Timestamp

case class Study(id : Int, creation : Timestamp, var title : String, var description : Option[String]) extends TableRow {
  override def hashCode = id
  override def equals(a : Any) = a match {
    case Study(i, _, _, _) => i == id
    case _ => false
  }

  def commit(implicit db : Session) =
    Study.byId(id).map(_.update_*) update (title, description)

  def access(p : Permission.Value = Permission.NONE)(implicit db : Session) : List[StudyAccess] = StudyAccess.getStudy(id, p)
  def check_access(i : Int)(implicit db : Session) : Permission.Value = StudyAccess.check(id, i)
  def check_access(e : Entity)(implicit db : Session) : Permission.Value = check_access(e.id)
}

object Study extends Table[Study]("study") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def created = column[Timestamp]("created")
  def title = column[String]("title", O.DBType("text"))
  def description = column[Option[String]]("description", O.DBType("text"))

  def * = id ~ created ~ title ~ description <> (Study.apply _, Study.unapply _)
  def update_* = title ~ description
  def insert_* = title

  private def byId(i : Int) = Query(this).where(_.id === i)

  def get(i : Int)(implicit db : Session) : Option[Study] =
    byId(i).firstOption

  def create(title : String)(implicit db : Session) : Study =
    insert_* returning * insert title
}

case class StudyAccess(studyId : Int, entityId : Int, var access : Permission.Value, var inherit : Permission.Value) extends TableRow {
  var id = (studyId, entityId)

  def commit(implicit db : Session) =
    StudyAccess.byKey(studyId, entityId).map(_.update_*) update (access, inherit)
  def add(implicit db : Session) =
    StudyAccess.* insert this
  def remove(implicit db : Session) =
    StudyAccess.delete(studyId, entityId)

  private val _study = CachedVal[Study](Study.get(studyId)(_).orNull)
  def study(implicit db : Session) : Study = _study
  private val _entity = CachedVal[Entity](Entity.get(entityId)(_))
  def entity(implicit db : Session) : Entity = _entity
}

object StudyAccess extends Table[StudyAccess]("study_access") {
  def studyId = column[Int]("study")
  def entityId = column[Int]("entity")
  def access = column[Permission.Value]("access")
  def inherit = column[Permission.Value]("inherit")

  def id = studyId ~ entityId
  def * = studyId ~ entityId ~ access ~ inherit <> (StudyAccess.apply _, StudyAccess.unapply _)
  def update_* = access ~ inherit

  def key = primaryKey("study_access_pkey", (studyId, entityId))
  def study = foreignKey("study_access_study_fkey", studyId, Study)(_.id)
  def entity = foreignKey("study_access_entity_fkey", entityId, Entity)(_.id)

  private def byKey(s : Int, e : Int) = Query(this).where(a => a.studyId === s && a.entityId === e)
  def byStudy(s : Int, p : Permission.Value = Permission.NONE) = 
    Query(this).where(a => a.studyId === s && a.access >= p)
  def byEntity(e : Int, p : Permission.Value = Permission.NONE) = 
    Query(this).where(a => a.entityId === e && a.access >= p)

  def get(s : Int, e : Int)(implicit db : Session) : Option[StudyAccess] =
    byKey(s, e).firstOption
  def getStudy(s : Int, p : Permission.Value = Permission.NONE)(implicit db : Session) : List[StudyAccess] =
    (for { 
      a <- byStudy(s, p).sortBy(_.access.desc) 
      e <- a.entity 
    } yield (a,e)).list.map({ case (a,e) =>
      a._entity() = e
      a
    })
  def getEntity(e : Int, p : Permission.Value = Permission.NONE)(implicit db : Session) : List[StudyAccess] =
    (for { 
      a <- byEntity(e, p).sortBy(_.access.desc) 
      s <- a.study 
    } yield (a,s)).list.map({ case (a,s) =>
      a._study() = s
      a
    })

  def delete(s : Int, e : Int)(implicit db : Session) =
    byKey(s, e).delete

  private[this] def _check = SimpleFunction.binary[Int, Int, Option[Permission.Value]]("study_access_check")
  def check(s : Int, e : Int)(implicit db : Session) : Permission.Value =
    Query(_check(s, e)).first.getOrElse(Permission.NONE)

  def filterForEntity(e : Int)(s : Column[Int]) = _check(s, e) >= Permission.VIEW
}
