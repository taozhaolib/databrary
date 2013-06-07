package models

import play.api.Play.current
import play.api.db.slick
import             slick.DB
import             slick.Config.driver.simple._
import java.sql.Timestamp
import util._

final class Study private (val id : Int, val creation : Timestamp, title_ : String, description_ : Option[String]) extends TableRow {
  override def hashCode = id
  override def equals(a : Any) = a match {
    case Study(i, _, _, _) => i == id
    case _ => false
  }

  private[this] var _title = title_
  def title = _title
  private[this] var _description = description_
  def description = _description

  private def * = (id, creation, title, description)

  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    implicit val db = site.db
    Study.byId(id).map(_.update_*) update (title, description)
    _title = title
    _description = description
    AuditStudy.add(AuditAction.change, this)
  }

  def access(p : Permission.Value = Permission.NONE)(implicit db : Session) : List[StudyAccess] = StudyAccess.getStudy(id, p)
  def check_access(i : Int)(implicit db : Session) : Permission.Value = StudyAccess.check(id, i)
  def check_access(i : Identity)(implicit db : Session) : Permission.Value = check_access(i.id)
}

object Study extends Table[Study]("study") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def created = column[Timestamp]("created")
  def title = column[String]("title", O.DBType("text"))
  def description = column[Option[String]]("description", O.DBType("text"))

  private[models] def apply(id : Int, creation : Timestamp, title : String, description : Option[String]) =
    new Study(id, creation, title, description)
  private[models] def unapply(s : Study) = Some(s.*)

  def * = id ~ created ~ title ~ description <> (apply _, unapply _)
  def update_* = title ~ description
  def insert_* = title

  private def byId(i : Int) = Query(this).where(_.id === i)

  def get(i : Int)(implicit db : Session) : Option[Study] =
    byId(i).firstOption

  def create(title : String)(implicit db : Session) : Study =
    insert_* returning * insert title
}

object AuditStudy extends AuditTable[Study](Study) {
  def id = column[Int]("id")
  def created = column[Timestamp]("created")
  def title = column[String]("title")
  def description = column[Option[String]]("description")

  def row = id ~ created ~ title ~ description <> (Study.apply _, Study.unapply _)
}

final case class StudyAccess(studyId : Int, entityId : Int, access : Permission.Value, inherit : Permission.Value) extends TableRow {
  val id = (studyId, entityId)

  def set(implicit site : Site) : Unit = {
    implicit val db = site.db
    val r = StudyAccess.byId(id).map(_.update_*) update (access, inherit)
    val act = if (r == 0) {
      StudyAccess.* insert this
      AuditAction.add
    } else
      AuditAction.change
    AuditStudyAccess.add(act, this)
  }
  def remove(implicit site : Site) : Unit = {
    implicit val db = site.db
    val r = StudyAccess.delete(id)
    if (r > 0)
      AuditStudyAccess.add(AuditAction.remove, this)
  }

  private val _study = CachedVal[Study](Study.get(studyId)(_).orNull)
  def study(implicit db : Session) : Study = _study
  private val _entity = CachedVal[Identity](Identity.get(entityId)(_))
  def entity(implicit db : Session) : Identity = _entity
}

object StudyAccess extends Table[StudyAccess]("study_access") {
  def studyId = column[Int]("study")
  def entityId = column[Int]("entity")
  def access = column[Permission.Value]("access")
  def inherit = column[Permission.Value]("inherit")

  type Id = (Int,Int)
  def id = studyId ~ entityId
  def * = studyId ~ entityId ~ access ~ inherit <> (apply _, unapply _)
  def update_* = access ~ inherit

  def key = primaryKey("study_access_pkey", (studyId, entityId))
  def study = foreignKey("study_access_study_fkey", studyId, Study)(_.id)
  private def entity = foreignKey("study_access_entity_fkey", entityId, Entity)(_.id)

  private def byKey(s : Int, e : Int) = Query(this).where(a => a.studyId === s && a.entityId === e)
  private def byId(i : Id) = byKey(i._1, i._2)
  def byStudy(s : Int, p : Permission.Value = Permission.NONE) = 
    Query(this).where(a => a.studyId === s && a.access >= p)
  def byEntity(e : Int, p : Permission.Value = Permission.NONE) = 
    Query(this).where(a => a.entityId === e && a.access >= p)

  def get(s : Int, e : Int)(implicit db : Session) : Option[StudyAccess] =
    byKey(s, e).firstOption
  def getStudy(s : Int, p : Permission.Value = Permission.NONE)(implicit db : Session) : List[StudyAccess] =
    /*(for {
      a <- */byStudy(s, p).sortBy(_.access.desc)/*
      i <- Identity.byEntity(a.entity) // triggers bug GH#159
    } yield (a,i))*/.list/*.map({ case (a,i) =>
      a._entity() = Identity.build(i)
      a
    })*/
  def getEntity(e : Int, p : Permission.Value = Permission.NONE)(implicit db : Session) : List[StudyAccess] =
    (for { 
      a <- byEntity(e, p).sortBy(_.access.desc) 
      s <- a.study 
    } yield (a,s)).list.map({ case (a,s) =>
      a._study() = s
      a
    })

  private def delete(i : Id)(implicit db : Session) =
    byId(i).delete

  private[this] def _check = SimpleFunction.binary[Int, Int, Option[Permission.Value]]("study_access_check")
  def check(s : Int, e : Int)(implicit db : Session) : Permission.Value =
    Query(_check(s, e)).first.getOrElse(Permission.NONE)

  def filterForEntity(e : Int)(s : Column[Int]) = _check(s, e) >= Permission.VIEW
}

object AuditStudyAccess extends AuditTable[StudyAccess](StudyAccess) {
  def studyId = column[Int]("study")
  def entityId = column[Int]("entity")
  def access = column[Permission.Value]("access")
  def inherit = column[Permission.Value]("inherit")

  def row = studyId ~ entityId ~ access ~ inherit <> (StudyAccess.apply _, StudyAccess.unapply _)
}
