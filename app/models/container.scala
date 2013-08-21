package models

import anorm._
import anorm.SqlParser.scalar
import java.sql.Date
import dbrary._
import dbrary.Anorm._
import util._

sealed abstract class Container protected (val id : Container.Id) extends TableRowId[Container] with SitePage with Annotated {
  /* Study owning this container (possibly itself) */
  def studyId : Study.Id
  def study : Study
  def permission : Permission.Value = study.permission
  def consent : Consent.Value

  def assets(implicit db : Site.DB) = AssetLink.getAssets(this)
  def getAsset(o : Asset.Id)(implicit db : Site.DB) = AssetLink.get(this, o)

  private[models] final def annotatedLevel = "container"
  private[models] final def annotatedId = id
}

final class Study private (override val id : Study.Id, title_ : String, description_ : Option[String], override val permission : Permission.Value) extends Container(id) with TableRowId[Study] {
  def studyId = id
  def study = this

  private[this] var _title = title_
  def title = _title
  private[this] var _description = description_
  def description = _description

  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    val args = 
    Audit.change("study", SQLArgs('title -> title, 'description -> description), SQLArgs('id -> id)).execute()(site.db)
    _title = title
    _description = description
  }

  def consent = Consent.NONE

  def pageName(implicit site : Site) = title
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Study.view(id).url

  def partyAccess(p : Permission.Value = Permission.NONE)(implicit db : Site.DB) = StudyAccess.getParties(this, p)

  def slots(implicit db : Site.DB) = Slot.getStudy(this)
}

final class Slot private (override val id : Slot.Id, val study : Study, val consent_ : Consent.Value, val date_ : Date) extends Container(id) with TableRowId[Slot] {
  def studyId = study.id
  private[this] var _consent = consent_
  def consent = _consent
  private[this] var _date = date_
  def date = _date

  def change(consent : Consent.Value = _consent, date : Date = _date)(implicit site : Site) : Unit = {
    if (date == _date && consent == _consent)
      return
    Audit.change("slot", SQLArgs('consent -> maybe(consent, Consent.NONE), 'date -> date), SQLArgs('id -> id)).execute()(site.db)
    _consent = consent
    _date = date
  }

  def pageName(implicit site : Site) = date.toString
  def pageParent(implicit site : Site) = Some(study)
  def pageURL = controllers.routes.Slot.view(id).url
}


private[models] sealed abstract class ContainerView[R <: Container with TableRowId[R]](table : String) extends TableId[R](table) {
  protected final val permission = "study_access_check(study.id, {identity})"
  private[models] final val condition = permission + " >= 'VIEW'"

  def get(i : Id)(implicit site : Site) : Option[R]
}

object Container extends ContainerView[Container]("container") {
  private[models] val row =
    (Study.row ~ Slot.columns.?) map {
      case (study ~ None) => study
      case (study ~ Some(slot)) => Slot.baseMake(study)(slot)
    }
  private[models] override val src = """container 
    LEFT JOIN slot USING (id) 
         JOIN study ON study.id = container.id OR study.id = slot.study"""
  def get(i : Id)(implicit site : Site) : Option[Container] =
    SELECT("WHERE container.id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()(site.db)

  def getAnnotation(annotation : Annotation)(implicit site : Site) : Seq[Container] =
    SELECT("JOIN container_annotations ON container.id = container WHERE annotation = {annotation}").
      on('annotation -> annotation.id, 'identity -> site.identity.id).list()(site.db)
}

object Study extends ContainerView[Study]("study") {
  private[models] val row = Columns[
    Id,  String, Option[String], Option[Permission.Value]](
    'id, 'title, 'description,   SelectAs(permission, "permission")) map {
    (id, title, description, permission) => new Study(id, title, description, permission.getOrElse(Permission.NONE))
  }

  def get(i : Id)(implicit site : Site) : Option[Study] =
    SELECT("WHERE id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()(site.db)
    
  def create(title : String, description : Option[String] = None)(implicit site : Site) : Study = {
    val id = Audit.add(table, SQLArgs('title -> title, 'description -> description), "id").single(scalar[Id])(site.db)
    new Study(id, title, description, Permission.NONE)
  }
}

object Slot extends ContainerView[Slot]("slot") {
  private[models] def makeStudy(study : Study)(id : Id, consent : Option[Consent.Value], date : Date) = new Slot(id, study, consent.getOrElse(Consent.NONE), date)
  private[models] def baseMake(study : Study) = (makeStudy(study) _).tupled
  private[models] val columns = Columns[
    Id,  Option[Consent.Value], Date](
    'id, 'consent,              'date)
  private[models] val row = (Study.row ~ columns) map {
    case (study ~ slot) => baseMake(study)(slot)
  }
  private[models] override val src = "slot JOIN " + Study.src + " ON slot.study = study.id"
  private[this] def rowStudy(study : Study) = columns.map(makeStudy(study) _)

  def get(i : Id)(implicit site : Site) : Option[Slot] =
    SELECT("WHERE slot.id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()(site.db)
  private[models] def getStudy(study : Study)(implicit db : Site.DB) : Seq[Slot] =
    SQL("SELECT " + columns.select + " FROM slot WHERE study = {study} ORDER BY date").
      on('study -> study.id).list(rowStudy(study))
    
  def create(study : Study, consent : Consent.Value, date : Date)(implicit site : Site) : Slot = {
    val id = Audit.add(table, SQLArgs('study -> study.id, 'consent -> maybe(consent, Consent.NONE), 'date -> date), "id").single(scalar[Id])(site.db)
    new Slot(id, study, consent, date)
  }
}
