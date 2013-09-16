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
final class Container protected (val id : Container.Id, val volume : Volume, val name_ : Option[String], val date_ : Option[Date]) extends TableRowId[Container] with SitePage with InVolume {
  def container = this
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

  /** List of contained assets within this container.
    * In most cases calling `fullSlot.assets` makes more sense. */
  def assets(implicit db : Site.DB) : Seq[ContainerAsset] = ContainerAsset.getContainer(this)

  /** List of slots on this container. */
  def slots(implicit db : Site.DB) : Seq[Slot] = Slot.getContainer(this)
  private val _fullSlot = CachedVal[Slot, Site.DB](Slot.getOrCreate(this)(_))
  /** Slot that covers this entire container and which thus serves as a proxy for display and metadata. Cached. */
  def fullSlot(implicit db : Site.DB) : Slot = _fullSlot

  def pageName(implicit site : Site) = date.toString // FIXME date permissions/useful title
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL = controllers.routes.Container.view(id).url
}

object Container extends TableId[Container]("container") {
  private[models] def make(volume : Volume)(id : Id, name : Option[String], date : Option[Date]) =
    new Container(id, volume, name, date)
  private[models] val columns = Columns[
    Id,  Option[String], Option[Date]](
    'id, 'name,          'date)
  private[models] val row = (Volume.row ~ columns) map {
    case (vol ~ cont) => (make(vol) _).tupled(cont)
  }
  private[models] def volumeRow(volume : Volume) = columns map (make(volume) _)
  private[models] override val src = "container JOIN volume ON container.volume = volume.id"

  /** Retrieve an individual Container.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Container] = {
    val cols = row ~ Slot.columns.? map { case (cont ~ slot) =>
      slot foreach { s =>
        cont._fullSlot() = (Slot.make(cont) _).tupled(s)
      }
      cont
    }
    SQL("SELECT " + cols.select + " FROM " + src + " LEFT JOIN " + Slot.baseSrc + " ON container.id = slot.source AND slot.segment = '(,)' WHERE container.id = {id} AND " + Volume.condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt(cols)
  }

  /** Retrieve all the containers in a given volume.
    * This does not check permissions as an existing volume implies visibility. */
  private[models] def getVolume(v : Volume)(implicit db : Site.DB) : Seq[Container] = {
    val row = volumeRow(v)
    SQL("SELECT " + row.select + " FROM container WHERE container.volume = {vol} ORDER BY date, id").
      on('vol -> v.id).list(row)
  }

  /** Create a new container in the specified volume. */
  def create(volume : Volume, name : Option[String] = None, date : Option[Date] = None)(implicit site : Site) = {
    val id = Audit.add(table, SQLArgs('volume -> volume.id, 'name -> name, 'date -> date), "id").single(scalar[Id])
    new Container(id, volume, name, date)
  }
}

/** Smallest organizatonal unit of related data.
  * Primarily used for an individual session of data with a single date and place.
  * Critically, contained data are should be covered by a single consent level and share the same annotations. */
final class Slot private (val id : Slot.Id, val container : Container, val segment : Range[Offset], consent_ : Consent.Value = Consent.NONE, toplevel_ : Boolean = false) extends TableRowId[Slot] with AnnotatedInVolume with SitePage {
  def containerId : Container.Id = container.id
  def volume = container.volume
  private[this] var _consent = consent_
  /** Effective consent for covered assets, or [[Consent.NONE]] if no matching consent. */
  def consent = _consent
  private[this] var _toplevel = toplevel_
  /** True if this slot has been promoted for toplevel display. */
  def toplevel = _toplevel

  /** Update the given values in the database and this object in-place. */
  def change(consent : Consent.Value = _consent, toplevel : Boolean = _toplevel)(implicit site : Site) : Unit = {
    if (consent != _consent)
    {
      Audit.change("slot", SQLArgs('consent -> consent), SQLArgs('id -> id)).execute()
      _consent = consent
    }
    if (toplevel != _toplevel)
    {
      if (toplevel)
        Audit.add("toplevel_slot", SQLArgs('slot -> id)).execute()
      else
        Audit.remove("toplevel_slot", SQLArgs('slot -> id)).execute()
    }
  }

  private[this] val _context = CachedVal[Option[Slot], Site.DB] { implicit db =>
    if (consent != Consent.NONE) Some(this) else {
      val row = Slot.containerRow(container)
      SQL("SELECT " + row.select + " FROM " + Slot.baseSrc + " WHERE slot.source = {cont} AND slot.segment @> {seg} AND slot.consent IS NOT NULL").
        on('cont -> containerId, 'seg -> segment).singleOpt(row)
    }
  }
  /** The relevant consented slot containing this one. Cached.
    * Users with FULL access fall back to the full container. */
  def context(implicit db : Site.DB) : Option[Slot] =
    _context.orElse(if (permission >= Permission.FULL) Some(container.fullSlot) else None)

  /** The level of access granted on data covered by this slot to the current user. */
  def dataPermission(classification : Classification.Value = Classification.RESTRICTED)(implicit site : Site) =
    Permission.data(permission, context.fold(consent)(_.consent), classification)

  /** Effective start point of this slot within the container. */
  def offset : Offset = segment.lowerBound.getOrElse(0)

  /** List of contained asset segments within this slot. */
  def assets(implicit db : Site.DB) : Seq[SlotAsset] = SlotAsset.getSlot(this)

  private[models] def annotatedLevel = "slot"
  private[models] def annotatedId = id

  def pageName(implicit site : Site) = this.toString // FIXME
  def pageParent(implicit site : Site) = Some(container)
  def pageURL = controllers.routes.Slot.view(id).url
}

object Slot extends TableId[Slot]("slot") {
  import PGSegment.{column => segmentColumn}
  private[models] def make(container : Container)(id : Id, segment : Range[Offset], consent : Option[Consent.Value], toplevel : Boolean) =
    new Slot(id, container, segment, consent.getOrElse(Consent.NONE), toplevel)
  private[models] val columns = Columns[
    Id,  Range[Offset], Option[Consent.Value], Boolean](
    'id, 'segment,      'consent,              SelectAs("toplevel_slot.slot IS NOT NULL", "toplevel"))
  private[models] val row = (Container.row ~ columns) map {
    case (cont ~ slot) => (make(cont) _).tupled(slot) 
  }
  private[models] def containerRow(container : Container) = columns map (make(container) _)
  private[models] val baseSrc = "slot LEFT JOIN toplevel_slot ON slot.id = toplevel_slot.slot"
  private[models] override val src = baseSrc + " JOIN " + Container.src + " ON slot.source = container.id"

  final val fullRange = Range.full[Offset](PGSegment)

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Slot] =
    SELECT("WHERE slot.id = {id} AND", Volume.condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve an individual Slot by Container and segment.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  private[models] def get(container : Container, segment : Range[Offset] = fullRange)(implicit db : Site.DB) : Option[Slot] = {
    val row = containerRow(container)
    SQL("SELECT " + row.select + " FROM " + baseSrc + " WHERE slot.source = {cont} AND slot.segment = {seg}").
      on('cont -> container.id, 'seg -> segment).singleOpt(row)
  }

  /** Retrieve a list of slots within the given container.
    * This does not check permissions as an existing volume implies visibility. */
  private[models] def getContainer(c : Container, top : Boolean = false)(implicit db : Site.DB) : Seq[Slot] = {
    val row = containerRow(c)
    SQL("SELECT " + row.select + " FROM slot " + (if (top) "" else "LEFT ") + "JOIN toplevel_slot ON slot.id = toplevel_slot.slot WHERE slot.source = {cont} ORDER BY slot.segment").
      on('cont -> c.id).list(row)
  }

  /** Retrieve a lost of slots with the given annotation.
    * This checks permissions, though in fact this is not necessary in all cases. FIXME. */
  private[models] def getAnnotation(a : Annotation.Id)(implicit site : Site) : Seq[Slot] =
    SELECT("JOIN slot_annotation ON slot.id = slot_annotation.slot WHERE slot_annotation.annotation = {annot} ORDER BY slot.source, slot.segment").
      on('annot -> a).list
    
  /** Create a new slot in the specified container or return a matching one if it already exists. */
  def getOrCreate(container : Container, segment : Range[Offset] = fullRange)(implicit db : Site.DB) : Slot =
    get(container, segment) getOrElse {
      val row = containerRow(container)
      SQL("INSERT INTO " + table + " " + SQLArgs('container -> container.id, 'segment -> segment).insert + " RETURNING " + row.select).single(row)
    }
}
