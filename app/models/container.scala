package models

import anorm._
import anorm.SqlParser.scalar
import java.sql.Date
import dbrary._
import dbrary.Anorm._
import PGSegment.{column => segmentColumn,statement => segmentStatement}
import util._

/** Collection of related assets.
  * To be used, all assets must be placed into containers.
  * These containers can represent a package of raw data acquired cotemporaneously or within a short time period (a single session), or a group of related materials.
  */
final class Container protected (val id : Container.Id, val volume : Volume, val top : Boolean = false, val name_ : Option[String], val date_ : Option[Date]) extends TableRowId[Container] with SitePage with InVolume {
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
  private[models] val _fullSlot = CachedVal[Slot, Site.DB](Slot.get(this)(_).get)
  /** Slot that covers this entire container and which thus serves as a proxy for display and metadata. Cached. */
  def fullSlot(implicit db : Site.DB) : Slot = _fullSlot

  def pageName(implicit site : Site) = date.toString // FIXME date permissions/useful title
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL = controllers.routes.Container.view(id).url
}

object Container extends TableId[Container]("container") {
  private[models] def make(volume : Volume)(id : Id, top : Boolean, name : Option[String], date : Option[Date]) =
    new Container(id, volume, top, name, date)
  private[models] val columns = Columns[
    Id,  Boolean, Option[String], Option[Date]](
    'id, 'top,    'name,          'date)
  private[models] val row = Volume.row.join(columns, "container.volume = volume.id") map {
    case (vol ~ cont) => (make(vol) _).tupled(cont)
  }
  private[models] def volumeRow(volume : Volume) = columns map (make(volume) _)

  /** Retrieve an individual Container.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Container] = {
    row.join(Slot.columns, "container.id = slot.source AND slot.segment = '(,)'").
      map { case (cont ~ slot) =>
        cont._fullSlot() = (Slot.make(cont) _).tupled(slot)
        cont
      }.SQL("WHERE container.id = {id} AND", Volume.condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt
  }

  /** Retrieve all the containers in a given volume. */
  private[models] def getVolume(v : Volume)(implicit db : Site.DB) : Seq[Container] =
    volumeRow(v).SQL("WHERE container.volume = {vol} ORDER BY date, id").
      on('vol -> v.id).list

  /** Retrieve the top container in a given volume. */
  private[models] def getTop(v : Volume)(implicit db : Site.DB) : Container =
    volumeRow(v).SQL("WHERE container.volume = {vol} AND top").
      on('vol -> v.id).single

  /** Create a new container in the specified volume. */
  def create(volume : Volume, name : Option[String] = None, date : Option[Date] = None)(implicit site : Site) = {
    val id = Audit.add(table, SQLArgs('volume -> volume.id, 'name -> name, 'date -> date), "id").single(scalar[Id])
    new Container(id, volume, false, name, date)
  }
}

/** Smallest organizatonal unit of related data.
  * Primarily used for an individual session of data with a single date and place.
  * Critically, contained data are should be covered by a single consent level and share the same annotations. */
final class Slot private (val id : Slot.Id, val container : Container, val segment : Range[Offset], consent_ : Consent.Value = Consent.NONE, toplevel_ : Boolean = false) extends TableRowId[Slot] with Commented with SitePage {
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
    if (consent != Consent.NONE || segment.isFull) Some(this) else {
      Slot.containerRow(container).SQL("WHERE slot.source = {cont} AND slot.segment @> {seg} AND slot.consent IS NOT NULL").
        on('cont -> containerId, 'seg -> segment).singleOpt()
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
  def position : Offset = segment.lowerBound.getOrElse(0)

  /** List of contained asset segments within this slot. */
  def assets(implicit db : Site.DB) : Seq[SlotAsset] = SlotAsset.getSlot(this)

  private[models] def commentSlot(implicit db : Site.DB) = this
  def comments(all : Boolean = true)(implicit db : Site.DB) : Seq[Comment] = Comment.getSlot(this, all)

  def tags(implicit site : Site) : Seq[TagWeight] = TagWeight.getSlot(this)

  /** The list of records on this object.
    * @param all include indirect records on any contained objects
    */
  def records(implicit db : Site.DB) : Seq[Record] = Record.getSlot(this)(db)
  /** Remove the given record from this slot. */
  def removeRecord(rec : Record.Id)(implicit db : Site.DB) : Unit = Record.removeSlot(rec, id)
  /** The list of records and possibly measures on this object.
    * This is essentially equivalent to `this.records(false).filter(_.category == category).map(r => (r, r.measure[T](metric)))` but more efficient.
    * @param category if Some limit to the given category */
  def recordMeasures[T](category : Option[RecordCategory] = None, metric : MetricT[T] = Metric.Ident)(implicit db : Site.DB) : Seq[(Record, Option[T])] =
    MeasureT.getSlot[T](this, category, metric)
  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  def idents(implicit db : Site.DB) : Seq[(String)] = recordMeasures() map {
    case (r, i) => r.category.fold("")(_.name + ':') + i.getOrElse("[" + r.id.unId.toString + ']')
  }

  def pageName(implicit site : Site) = this.toString // FIXME
  def pageParent(implicit site : Site) = Some(container)
  def pageURL = controllers.routes.Slot.view(id).url
}

object Slot extends TableId[Slot]("slot") {
  private[models] def make(container : Container)(id : Id, segment : Range[Offset], consent : Option[Consent.Value], toplevel : Boolean) =
    new Slot(id, container, segment, consent.getOrElse(Consent.NONE), toplevel)
  private def columnsTop(top : Boolean = false) = Columns[
    Id,  Range[Offset], Option[Consent.Value], Boolean](
    'id, 'segment,      'consent,              SelectAs("toplevel_slot.slot IS NOT NULL", "toplevel")).
    from("slot " + (if (top) "" else "LEFT ") + "JOIN toplevel_slot ON slot.id = toplevel_slot.slot")
  private[models] val columns = columnsTop()
  private[models] val row = columns.join(Container.row, "slot.source = container.id") map {
    case (slot ~ cont) => (make(cont) _).tupled(slot) 
  }
  private[models] def containerRow(container : Container, top : Boolean = false) =
    columnsTop(top) map (make(container) _).tupled
  private[models] def volumeColumns(top : Boolean = false) =
    columnsTop(top).join(Container.columns, "slot.source = container.id")
  private[models] def volumeRow(volume : Volume, top : Boolean = false) =
    volumeColumns(top) map {
      case (slot ~ cont) => (make((Container.make(volume) _).tupled(cont)) _).tupled(slot)
    }

  final val fullRange = Range.full[Offset](PGSegment)

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Slot] =
    row.SQL("WHERE slot.id = {id} AND", Volume.condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt

  /** Retrieve an individual Slot by Container and segment.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  private[models] def get(container : Container, segment : Range[Offset] = fullRange)(implicit db : Site.DB) : Option[Slot] =
    containerRow(container).
      SQL("WHERE slot.source = {cont} AND slot.segment = {seg}").
      on('cont -> container.id, 'seg -> segment).singleOpt

  /** Retrieve a list of slots within the given container. */
  private[models] def getContainer(c : Container, top : Boolean = false)(implicit db : Site.DB) : Seq[Slot] =
    containerRow(c, top = top).
      SQL("WHERE slot.source = {cont} ORDER BY slot.segment").
      on('cont -> c.id).list

  /** Retrieve the master slot for a volume. */
  private[models] def getTop(v : Volume)(implicit db : Site.DB) : Slot = {
    volumeRow(v).
      SQL("WHERE slot.segment = '(,)' AND container.volume = {vol} AND container.top").
      on('vol -> v.id).single
  }

  /** Create a new slot in the specified container or return a matching one if it already exists. */
  def getOrCreate(container : Container, segment : Range[Offset])(implicit db : Site.DB) : Slot =
    DBUtil.selectOrInsert(get(container, segment)) {
      val args = SQLArgs('source -> container.id, 'segment -> segment)
      val id = SQL("INSERT INTO slot " + args.insert + " RETURNING id").
        on(args : _*).single(scalar[Id])
      new Slot(id, container, segment)
    }
}
