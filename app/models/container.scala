package models

import anorm._
import anorm.SqlParser.scalar
import java.sql.Date
import dbrary._
import dbrary.Anorm._
import util._

/** Collection of related assets.
  * To be used, all assets must be placed into containers.
  * These containers can represent a package of raw data acquired cotemporaneously or within a short time period (a single session), or a group of related materials.
  */
final class Container protected (val id : Container.Id, val volume : Volume, val name_ : Option[String], val date_ : Option[Date]) extends TableRowId[Container] with SitePage {
  def volumeId = volume.id
  private[this] var _name = name_
  /** Descriptive name to help with organization by contributors.
    * This (as with Container in general) is not necessarily intended for public consumption. */
  def name = _name
  private[this] var _date = date_
  /** The date at which the contained data were collected.
    * Note that this is covered (in part) by dataAccess permissions due to birthday/age restrictions. */
  def date = _date

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[String] = _name, date : Option[Date] = _date)(implicit site : Site) : Unit = {
    if (name == _name && date == _date)
      return
    Audit.change("container", SQLArgs('name -> name, 'date -> date), SQLArgs('id -> id)).execute()
    _name = name
    _date = date
  }

  /** Permission granted to the current site user for this container, defined by the containing volume and determined at lookup time. */
  def permission : Permission.Value = volume.permission

  private[this] val _assets = CachedVal[Seq[AssetLink], Site.DB](AssetLink.getAssets(this)(_))
  /** List of contained assets within this container. Cached. */
  def assets(implicit db : Site.DB) : Seq[AssetLink] = _assets
  /** Look up a specific contained asset. */
  def getAsset(o : Asset.Id)(implicit db : Site.DB) = AssetLink.get(this, o)

  /** List of slots on this container. */
  def slots(implicit db : Site.DB) : Seq[Slot] = Slot.getContainer(this)

  def pageName(implicit site : Site) = date.toString // FIXME date permissions/useful title
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL = controllers.routes.Container.view(id).url
}

object Container extends TableId[Container]("container") {
  private def make(volume : Volume)(id : Id, name : Option[String], date : Option[Date]) =
    new Container(id, volume, name, date)
  private[models] val columns = Columns[
    Id,  Option[String], Option[Date]](
    'id, 'name,          'date)
  private[models] val row = (Volume.row ~ columns) map {
    case (vol ~ cont) => (make(vol) _).tupled(cont)
  }
  private[models] override val src = "container JOIN volume ON container.volume = volume.id"

  /** Retrieve an individual Container.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Container] =
    SELECT("WHERE container.id = {id} AND", Volume.condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve all the containers in a given volume.
    * This does not check permissions as an existing volume implies visibility. */
  private[models] def getVolume(v : Volume)(implicit db : Site.DB) : Seq[Container] =
    SQL("SELECT " + columns.select + " FROM container WHERE container.volume = {vol} ORDER BY date, id").
      on('vol -> v.id).list(columns.map(make(v) _))

  /** Create a new container in the specified volume. */
  def create(volume : Volume, name : Option[String] = None, date : Option[Date] = None)(implicit site : Site) : Slot = {
    val id = Audit.add(table, SQLArgs('volume -> volume.id, 'name -> name, 'date -> date), "id").single(scalar[Id])
    new Container(id, volume, name, date)
  }
}

/** Smallest organizatonal unit of related data.
  * Primarily used for an individual session of data with a single date and place.
  * Critically, contained data are should be covered by a single consent level and share the same annotations. */
final class Slot private (val id : Slot.Id, val container : Container, val segment : Range[Offset], val consent_ : Consent.Value) extends TableRowId[Slot] with Annotated {
  def volumeId = volume.id
  private[this] var _consent = consent_
  def consent = _consent

  /** Update the given values in the database and this object in-place. */
  def change(consent : Consent.Value = _consent)(implicit site : Site) : Unit = {
    if (consent == _consent)
      return
    val ids = SQLArgs('slot -> id)
    if (consent == Consent.NONE)
      Audit.remove("slot_consent", ids).execute
    else if (Audit.change("slot_consent", SQLArgs('consent -> consent), idt).executeUpdate == 0)
      Audit.add("slot_consent", SQLArgs('consent -> consent), ids).execute
    _consent = consent
  }

  /** Effective start point of this slot within the container. */
  def offset : Offset = segment.lowerBound.getOrElse(0)

  /** Permission granted to the current site user for this container, defined by the containing volume and determined at lookup time. */
  def permission : Permission.Value = consent.permission

  /** The level of access granted on data covered by this slot to the current user. */
  def dataPermission(classification : Classification.Value = Classification.RESTRICTED)(implicit site : Site) =
    Permission.data(permission, consent, classification)

  /** List of contained asset segments within this slot. */
  def assets(implicit db : Site.DB) : Seq[AssetLink] = _assets

  private[models] def annotatedLevel = "slot"
  private[models] def annotatedId = id

  def pageName(implicit site : Site) = this.toString // FIXME
  def pageParent(implicit site : Site) = Some(container)
  def pageURL = controllers.routes.Slot.view(id).url
}

object Slot extends TableId[Slot]("slot") {
  private def make(container : Container)(id : Id, segment : Range[Offset], consent : Option[Consent.Value]) =
    new Slot(id, container, segment, consent.getOrElse(Consent.NONE))
  private[models] val columns = Columns[
    Id,  Range[Offset], Option[Consent.Value]](
    'id, 'segment,      'consent)
  private[models] val row = (Container.row ~ columns) map {
    case (cont ~ slot) => (make(cont) _).tupled(slot) 
  }
  private val baseSrc = "slot LEFT JOIN slot_consent ON slot.id = slot_consent.slot"
  private[models] override val src = baseSrc + " JOIN " + Container.src + " ON slot.source = container.id"

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Slot] =
    SELECT("WHERE slot.id = {id} AND", Volume.condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve a list of slots within the given container.
    * This does not check permissions as an existing volume implies visibility. */
  private[models] def getContainer(c : Container)(implicit db : Site.DB) : Seq[Slot] =
    SQL("SELECT " + columns.select + " FROM " + baseSrc + " WHERE slot.source = {cont} ORDER BY slot.segment").
      on('cont -> c.id).list(columns.map(make(c) _))
    
  /** Create a new slot in the specified container. */
  def create(container : Container, segment : Range[Offset] = Range.full[Offset])(implicit site : Site) : Slot = {
    val id = Audit.add(table, SQLArgs('container -> container.id, 'segment -> segment), "id").single(scalar[Id])
    new Slot(id, container, segment)
  }
}
