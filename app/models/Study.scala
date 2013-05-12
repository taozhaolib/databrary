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

  def commit = DB.withSession { implicit session =>
    Study.byId(id).map(_.mutable) update (title, description)
  }

  def access(p : Permission.Value = Permission.NONE) : List[StudyAccess] = StudyAccess.getStudy(id, p)
  def check_access(i : Int) : Permission.Value = StudyAccess.check(id, i)
  def check_access(e : Entity) : Permission.Value = check_access(e.id)
}

object Study extends Table[Study]("study") {
  def id = column[Int]("id")
  def created = column[Timestamp]("created")
  def title = column[String]("title", O.DBType("text"))
  def description = column[Option[String]]("description", O.DBType("text"))

  def * = id ~ created ~ title ~ description <> (Study.apply _, Study.unapply _)
  def mutable = title ~ description

  def byId(i : Int) = Query(this).where(_.id === i)

  def get(i : Int) : Option[Study] = DB.withSession { implicit session =>
    byId(i).firstOption
  }
}

case class StudyAccess(studyId : Int, entityId : Int, var access : Permission.Value, var inherited : Boolean) extends TableRow {

  def commit = DB.withSession { implicit session =>
    StudyAccess.byKey(studyId, entityId).map(_.mutable) update (access, inherited)
  }
  def add = DB.withSession { implicit session =>
    StudyAccess.* insert this
  }
  def remove = DB.withSession { implicit session =>
    StudyAccess.delete(studyId, entityId)
  }

  private val _study = CachedVal[Study](Study.get(studyId).orNull)
  def study : Study = _study
  private val _entity = CachedVal[Entity](Entity.get(entityId))
  def entity : Entity = _entity
}

object StudyAccess extends Table[StudyAccess]("study_access") {
  def studyId = column[Int]("study")
  def entityId = column[Int]("entity")
  def access = column[Permission.Value]("access")
  def inherited = column[Boolean]("inherited")

  def * = studyId ~ entityId ~ access ~ inherited <> (StudyAccess.apply _, StudyAccess.unapply _)
  def mutable = access ~ inherited

  def key = primaryKey("study_access_pkey", (studyId, entityId))
  def study = foreignKey("study_access_study_fkey", studyId, Study)(_.id)
  def entity = foreignKey("study_access_entity_fkey", entityId, Entity)(_.id)

  def byKey(s : Int, e : Int) = Query(this).where(a => a.studyId === s && a.entityId === e)
  def byStudy(s : Int, p : Permission.Value = Permission.NONE) = 
    Query(this).where(a => a.studyId === s && a.access >= p)
  def byEntity(e : Int, p : Permission.Value = Permission.NONE) = 
    Query(this).where(a => a.entityId === e && a.access >= p)

  def get(s : Int, e : Int) : Option[StudyAccess] = DB.withSession { implicit session =>
    byKey(s, e).firstOption
  }
  def getStudy(s : Int, p : Permission.Value = Permission.NONE) : List[StudyAccess] = DB.withSession { implicit session =>
    (for { a <- byStudy(s, p).sortBy(_.access.desc) ; e <- a.entity } yield (a,e)).list.map({ case (a,e) =>
      a._entity() = e
      a
    })
  }
  def getEntity(e : Int, p : Permission.Value = Permission.NONE) : List[StudyAccess] = DB.withSession { implicit session =>
    (for { a <- byEntity(e, p).sortBy(_.access.desc) ; s <- a.study } yield (a,s)).list.map({ case (a,s) =>
      a._study() = s
      a
    })
  }
  def delete(s : Int, e : Int) = DB.withSession { implicit session =>
    byKey(s, e).delete
  }

  def _check = SimpleFunction.binary[Int, Int, Option[Permission.Value]]("study_access_check")
  def check(s : Int, e : Int) : Permission.Value = DB.withSession { implicit session =>
    Query(_check(s, e)).first.getOrElse(Permission.NONE)
  }

  def filterForEntity(e : Int)(s : Column[Int]) = _check(s, e) >= Permission.VIEW
}
