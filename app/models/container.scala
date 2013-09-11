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
  /** Volume owning this container (which may be itself). */
  def volumeId : Volume.Id
  /** Volume owning this container (which may be itself). */
  def volume : Volume
  /** Permission granted to the current site user for this container, defined by the containing volume and determined at lookup time.
    * This will never be less than [[Permission.VIEW]] except for newly created volumes, as such objects would not be returned in the first place. */
  def permission : Permission.Value = volume.permission
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
final class Volume private (override val id : Volume.Id, title_ : String, description_ : Option[String], override val permission : Permission.Value) extends Container(id) with TableRowId[Volume] {
  def volumeId = id
  def volume = this

  private[this] var _title = title_
  /** Title headline of this volume. */
  def title = _title
  private[this] var _description = description_
  /** Longer, abstract-like description of this volume. */
  def description = _description

  /** Update the given values in the database and this object in-place. */
  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    val args = 
    Audit.change("volume", SQLArgs('title -> title, 'description -> description), SQLArgs('id -> id)).execute()
    _title = title
    _description = description
  }

  /** Participant consent level granted on contained data.
    * Always [[Consent.NONE]] for volumes as shared raw data should live elsewhere. */
  def consent = Consent.NONE

  def pageName(implicit site : Site) = title
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Volume.view(id).url

  /** List of parties which have at least the specified level of access to this volume. */
  def partyAccess(p : Permission.Value = Permission.NONE)(implicit db : Site.DB) = VolumeAccess.getParties(this, p)

  /** List of slots within this volume. */
  def slots(implicit db : Site.DB) = Slot.getVolume(this)

  /** Get volume creation information */
  def creationAudit(implicit db : Site.DB) : Option[Audit[Unit]] = {
    def cols = Audit.row[Unit]((), "audit_volume")
    SQL("SELECT " + cols.select + " FROM audit_volume WHERE id = {id} AND action = 'add'").
      on('id -> id).singleOpt(cols)
  }

  /** List of records associated with any slot in this volume.
    * @param category restrict to the specified category
    * @return unique records sorted by category */
  def slotRecords(category : Option[RecordCategory] = None)(implicit db : Site.DB) = Record.getSlots(this, category)
}

/** Smallest organizatonal unit of related data, primarily used for an individual session of data with a single date, place, and consent level. */
final class Slot private (override val id : Slot.Id, val volume : Volume, val consent_ : Consent.Value, val date_ : Date) extends Container(id) with TableRowId[Slot] {
  def volumeId = volume.id
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
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL = controllers.routes.Slot.view(id).url

  /** The level of access granted on data covered by this slot to the current user. */
  def dataAccess(classification : Classification.Value = Classification.RESTRICTED)(implicit site : Site) =
    Permission.data(permission, consent, classification)
}


/** Base for container interfaces. */
private[models] sealed abstract class ContainerView[R <: Container with TableRowId[R]](table : String) extends TableId[R](table) {
  protected final val permission = "volume_access_check(volume.id, {identity})"
  private[models] final val condition = permission + " >= 'VIEW'"

  def get(i : Id)(implicit site : Site) : Option[R]
}

object Container extends ContainerView[Container]("container") {
  private[models] val row =
    (Volume.row ~ Slot.columns.?) map {
      case (volume ~ None) => volume
      case (volume ~ Some(slot)) => Slot.baseMake(volume)(slot)
    }
  private[models] override val src = """container 
    LEFT JOIN slot USING (id) 
         JOIN volume ON volume.id = container.id OR volume.id = slot.volume"""

  /** Retrieve an individual Container, according to its type.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Container] =
    SELECT("WHERE container.id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve the set of containers to which the given annotation is attached.
    * @return viewable containers ordered by volume, date */
  def getAnnotation(annotation : Annotation)(implicit site : Site) : Seq[Container] =
    SELECT("JOIN container_annotation ON container.id = container WHERE annotation = {annotation} ORDER BY volume.id, slot.date, slot.id").
      on('annotation -> annotation.id, 'identity -> site.identity.id).list()
}

object Volume extends ContainerView[Volume]("volume") {
  private[models] val row = Columns[
    Id,  String, Option[String], Option[Permission.Value]](
    'id, 'title, 'description,   SelectAs(permission, "permission")) map {
    (id, title, description, permission) => new Volume(id, title, description, permission.getOrElse(Permission.NONE))
  }

  /** Retrieve an individual Volume.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Volume] =
    SELECT("WHERE id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve the set of all volumes in the system.
    * This only returns volumes for which the current user has [[Permission.VIEW]] access. */
  def getAll(implicit site : Site) : Seq[Volume] =
    SELECT("WHERE", condition).
      on('identity -> site.identity.id).list()
    
  /** Create a new, empty volume with no permissions.
    * The caller should probably add a [[VolumeAccess]] for this volume to grant [[Permission.ADMIN]] access to some user. */
  def create(title : String, description : Option[String] = None)(implicit site : Site) : Volume = {
    val id = Audit.add(table, SQLArgs('title -> title, 'description -> description), "id").single(scalar[Id])
    new Volume(id, title, description, Permission.NONE)
  }
}

object Slot extends ContainerView[Slot]("slot") {
  private[models] def makeVolume(volume : Volume)(id : Id, consent : Option[Consent.Value], date : Date) = new Slot(id, volume, consent.getOrElse(Consent.NONE), date)
  private[models] def baseMake(volume : Volume) = (makeVolume(volume) _).tupled
  private[models] val columns = Columns[
    Id,  Option[Consent.Value], Date](
    'id, 'consent,              'date)
  private[models] val row = (Volume.row ~ columns) map {
    case (volume ~ slot) => baseMake(volume)(slot)
  }
  private[models] override val src = "slot JOIN " + Volume.src + " ON slot.volume = volume.id"
  private[this] def rowVolume(volume : Volume) = columns.map(makeVolume(volume) _)

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Slot] =
    SELECT("WHERE slot.id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()
  /** Retrieve a list of slots within th egiven volume. */
  private[models] def getVolume(volume : Volume)(implicit db : Site.DB) : Seq[Slot] =
    SQL("SELECT " + columns.select + " FROM slot WHERE volume = {volume} ORDER BY date").
      on('volume -> volume.id).list(rowVolume(volume))
    
  /** Create a new slot in the specified volume. */
  def create(volume : Volume, consent : Consent.Value, date : Date)(implicit site : Site) : Slot = {
    val id = Audit.add(table, SQLArgs('volume -> volume.id, 'consent -> maybe(consent, Consent.NONE), 'date -> date), "id").single(scalar[Id])
    new Slot(id, volume, consent, date)
  }
}
