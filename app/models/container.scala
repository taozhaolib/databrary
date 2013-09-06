package models

import anorm._
import anorm.SqlParser.scalar
import java.sql.Date
import dbrary._
import dbrary.Anorm._
import util._

/** Base class for all objects which can contain or serve as attachment point for assets.
  * These objects, as they include user-specific permissions, are only valid for a single request. */
sealed abstract class Container protected (val id : Container.Id) extends TableRowId[Container] with SitePage with Annotated {
  /** Study owning this container (which may be itself). */
  def studyId : Study.Id
  /** Study owning this container (which may be itself). */
  def study : Study
  /** Permission granted to the current site user for this container, defined by the containing study and determined at lookup time.
    * This will never be less than [[Permission.VIEW]] except for newly created studies, as such objects would not be returned in the first place. */
  def permission : Permission.Value = study.permission
  /** Participant consent level granted on contained data, or [[Consent.NONE]]. */
  def consent : Consent.Value

  /** List of contained assets within this container. */
  private[this] val _assets = CachedVal[Seq[AssetLink], Site.DB](AssetLink.getAssets(this)(_))
  def assets(implicit db : Site.DB) : Seq[AssetLink] = _assets
  /** Look up a specific contained asset. */
  def getAsset(o : Asset.Id)(implicit db : Site.DB) = AssetLink.get(this, o)

  private[models] final def annotatedLevel = "container"
  private[models] final def annotatedId = id
}

/** Main organizational unit or package of data, within which everything else exists.
  * Usually represents a single project or dataset with a single set of procedures. */
final class Study private (override val id : Study.Id, title_ : String, description_ : Option[String], override val permission : Permission.Value) extends Container(id) with TableRowId[Study] {
  def studyId = id
  def study = this

  private[this] var _title = title_
  /** Title headline of this study. */
  def title = _title
  private[this] var _description = description_
  /** Longer, abstract-like description of this study. */
  def description = _description

  /** Update the given values in the database and this object in-place. */
  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    val args = 
    Audit.change("study", SQLArgs('title -> title, 'description -> description), SQLArgs('id -> id)).execute()
    _title = title
    _description = description
  }

  /** Participant consent level granted on contained data.
    * Always [[Consent.NONE]] for studies as shared raw data should live elsewhere. */
  def consent = Consent.NONE

  def pageName(implicit site : Site) = title
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Study.view(id).url

  /** List of parties which have at least the specified level of access to this study. */
  def partyAccess(p : Permission.Value = Permission.NONE)(implicit db : Site.DB) = StudyAccess.getParties(this, p)

  /** List of slots within this study. */
  def slots(implicit db : Site.DB) = Slot.getStudy(this)

  /** Get study creation information */
  def creationAudit(implicit db : Site.DB) : Option[Audit[Unit]] = {
    def cols = Audit.row[Unit]((), "audit_study")
    SQL("SELECT " + cols.select + " FROM audit_study WHERE id = {id} AND action = 'add'").
      on('id -> id).singleOpt(cols)
  }

  /** List of records associated with any slot in this study.
    * @param category restrict to the specified category
    * @return unique records sorted by category */
  def slotRecords(category : Option[RecordCategory] = None)(implicit db : Site.DB) = Record.getSlots(this, category)
}

/** Smallest organizatonal unit of related data, primarily used for an individual session of data with a single date, place, and consent level. */
final class Slot private (override val id : Slot.Id, val study : Study, val consent_ : Consent.Value, val date_ : Date) extends Container(id) with TableRowId[Slot] {
  def studyId = study.id
  private[this] var _consent = consent_
  def consent = _consent
  private[this] var _date = date_
  /** The date at which the contained data were collected.
    * Note that this is covered (in part) by dataAccess permissions due to birthday/age restrictions. */
  def date = _date

  /** Update the given values in the database and this object in-place. */
  def change(consent : Consent.Value = _consent, date : Date = _date)(implicit site : Site) : Unit = {
    if (date == _date && consent == _consent)
      return
    Audit.change("slot", SQLArgs('consent -> maybe(consent, Consent.NONE), 'date -> date), SQLArgs('id -> id)).execute()
    _consent = consent
    _date = date
  }

  def pageName(implicit site : Site) = date.toString
  def pageParent(implicit site : Site) = Some(study)
  def pageURL = controllers.routes.Slot.view(id).url

  /** The level of access granted on data covered by this slot to the current user. */
  def dataAccess(classification : Classification.Value = Classification.RESTRICTED)(implicit site : Site) =
    Permission.data(permission, consent, classification)
}


/** Base for container interfaces. */
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

  /** Retrieve an individual Container, according to its type.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Container] =
    SELECT("WHERE container.id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve the set of containers to which the given annotation is attached.
    * @return viewable containers ordered by study, date */
  def getAnnotation(annotation : Annotation)(implicit site : Site) : Seq[Container] =
    SELECT("JOIN container_annotation ON container.id = container WHERE annotation = {annotation} ORDER BY study.id, slot.date, slot.id").
      on('annotation -> annotation.id, 'identity -> site.identity.id).list()
}

object Study extends ContainerView[Study]("study") {
  private[models] val row = Columns[
    Id,  String, Option[String], Option[Permission.Value]](
    'id, 'title, 'description,   SelectAs(permission, "permission")) map {
    (id, title, description, permission) => new Study(id, title, description, permission.getOrElse(Permission.NONE))
  }

  /** Retrieve an individual Study.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Study] =
    SELECT("WHERE id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve the set of all studies in the system.
    * This only returns studies for which the current user has [[Permission.VIEW]] access. */
  def getAll(implicit site : Site) : Seq[Study] =
    SELECT("WHERE", condition).
      on('identity -> site.identity.id).list()
    
  /** Create a new, empty study with no permissions.
    * The caller should probably add a [[StudyAccess]] for this study to grant [[Permission.ADMIN]] access to some user. */
  def create(title : String, description : Option[String] = None)(implicit site : Site) : Study = {
    val id = Audit.add(table, SQLArgs('title -> title, 'description -> description), "id").single(scalar[Id])
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

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Slot] =
    SELECT("WHERE slot.id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()
  /** Retrieve a list of slots within th egiven study. */
  private[models] def getStudy(study : Study)(implicit db : Site.DB) : Seq[Slot] =
    SQL("SELECT " + columns.select + " FROM slot WHERE study = {study} ORDER BY date").
      on('study -> study.id).list(rowStudy(study))
    
  /** Create a new slot in the specified study. */
  def create(study : Study, consent : Consent.Value, date : Date)(implicit site : Site) : Slot = {
    val id = Audit.add(table, SQLArgs('study -> study.id, 'consent -> maybe(consent, Consent.NONE), 'date -> date), "id").single(scalar[Id])
    new Slot(id, study, consent, date)
  }
}
