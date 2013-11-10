package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** Collection of related assets.
  * To be used, all assets must be placed into containers.
  * These containers can represent a package of raw data acquired cotemporaneously or within a short time period (a single session), or a group of related materials.
  */
final class Container protected (val id : Container.Id, val volume : Volume, val top : Boolean = false, val name_ : Option[String], val date_ : Option[Date]) extends TableRowId[Container] with SiteObject with InVolume {
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
  def change(name : Option[String] = _name, date : Option[Date] = _date) : Future[Boolean] = {
    if (name == _name && date == _date)
      return Async(true)
    Audit.change("container", SQLTerms('name -> name, 'date -> date), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        _name = name
        _date = date
      }
  }

  /** List of contained assets within this container.
    * In most cases calling `fullSlot.assets` makes more sense. */
  def assets : Future[Seq[ContainerAsset]] = ContainerAsset.getContainer(this)

  /** List of slots on this container. */
  def slots : Future[Seq[Slot]] = Slot.getContainer(this)
  private[models] val _fullSlot = FutureVar[Slot] {
    Slot.containerRow(this)
      .SELECT("WHERE slot.source = ? AND slot.segment = '(,)'")
      .apply(id).single
  }
  /** Slot that covers this entire container and which thus serves as a proxy for display and metadata. Cached. */
  def fullSlot : Future[Slot] = _fullSlot.apply

  def pageName = _fullSlot.get.pageName
  def pageParent = Some(volume)
  def pageURL = _fullSlot.get.pageURL
  def pageActions = _fullSlot.get.pageActions
}

object Container extends TableId[Container]("container") {
  private[models] def make(volume : Volume, fullSlot : (Slot.Id, Range[Offset], Consent.Value))(id : Id, top : Boolean, name : Option[String], date : Option[Date]) : Container = {
    val c = new Container(id, volume, top, name, date)
    c._fullSlot.set((Slot.make(c) _).tupled(fullSlot))
    c
  }
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Boolean]("top")
    , SelectColumn[Option[String]]("name")
    , SelectColumn[Option[Date]]("date")
    )
  private val fullColumns = columns.join(Slot.columns.fromAlias("full_slot"), "full_slot.source = container.id AND full_slot.segment = '(,)'")
  private[models] def row(implicit site : Site) =
    Volume.row.join(fullColumns, "container.volume = volume.id") map {
      case (vol, (cont, full)) => (make(vol, full) _).tupled(cont)
    }
  private[models] def volumeRow(volume : Volume) =
    fullColumns map {
      case (cont, full) => (make(volume, full) _).tupled(cont)
    }

  /** Retrieve an individual Container.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Container]] =
    row.SELECT("WHERE container.id = ? AND", Volume.condition)
      .apply(i +: Volume.conditionArgs).singleOpt

  /** Retrieve all the (non-top) containers in a given volume. */
  private[models] def getVolume(v : Volume) : Future[Seq[Container]] =
    volumeRow(v).SELECT("WHERE container.volume = ? AND NOT top ORDER BY date, id")
      .apply(v.id).list

  /** Retrieve the top container in a given volume. */
  private[models] def getTop(v : Volume) : Future[Container] =
    volumeRow(v).SELECT("WHERE container.volume = ? AND top")
      .apply(v.id).single

  /** Find the containers in a given volume with the given name. */
  def findName(v : Volume, name : String) : Future[Seq[Container]] =
    volumeRow(v).SELECT("WHERE container.volume = ? AND container.name = ?")
      .apply(v.id, name).list

  /** Create a new container in the specified volume. */
  def create(volume : Volume, name : Option[String] = None, date : Option[Date] = None)(implicit site : Site) : Future[Container] =
    Audit.add(table, SQLTerms('volume -> volume.id, 'name -> name, 'date -> date), "id")
      .single(SQLCols[Id].map(new Container(_, volume, false, name, date)))
}

/** Smallest organizatonal unit of related data.
  * Primarily used for an individual session of data with a single date and place.
  * Critically, contained data are should be covered by a single consent level and share the same annotations. */
final class Slot private (val id : Slot.Id, val container : Container, val segment : Range[Offset], consent_ : Consent.Value = Consent.NONE) extends TableRowId[Slot] with InVolume with SiteObject {
  def containerId : Container.Id = container.id
  def volume = container.volume
  private[this] var _consent = consent_
  /** Effective consent for covered assets, or [[Consent.NONE]] if no matching consent. */
  def consent = _consent
  /** True if this is its container's full slot. */
  def isFull = segment.isFull

  /** Update the given values in the database and this object in-place. */
  def change(consent : Consent.Value = _consent) : Future[Boolean] = {
    if (consent == _consent)
      return Async(true)
    Audit.change("slot", SQLTerms('consent -> Maybe(consent).opt), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        _consent = consent
      }
  }

  def isContext : Boolean = consent != Consent.NONE || isFull

  /** The relevant consented slot containing this one.
    * Defaults to fullSlot. */
  lazy val context : Future[Slot] = {
    if (isContext) Async(this)
    else Async.getOrElse(
      container._fullSlot.peek.filter(_.consent != Consent.NONE), // optimization
      Slot.containerRow(container).SELECT("WHERE slot.source = ? AND slot.segment @> ? AND slot.consent IS NOT NULL")
        .apply(containerId, segment).singleOpt.flatMap(
          Async.getOrElse(_, container.fullSlot)
        )
    )
  }

  def getConsent : Consent.Value = Async.get(context).consent

  /** The level of access granted on data covered by this slot to the current user. */
  def dataPermission(classification : Classification.Value = Classification.RESTRICTED) : HasPermission =
    Permission.data(volume.permission, implicit site => getConsent, classification)

  /** Effective start point of this slot within the container. */
  def position : Offset = segment.lowerBound.getOrElse(0)

  /** List of contained asset segments within this slot. */
  def assets : Future[Seq[SlotAsset]] = SlotAsset.getSlot(this)

  /** The list of comments on this object.
    * @param all include indirect comments on any contained objects
    */
  def comments(all : Boolean = true) = Comment.getSlot(this, all)
  /** Post a new comment this object.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  def postComment(text : String)(implicit site : AuthSite) : Future[Comment] = Comment.post(this, text)

  /** The list of tags on the current slot along with the current user's applications.
    * @param all add any tags applied to child slots to weight (but not use) as well */
  def tags(all : Boolean = true) = TagWeight.getSlot(this, all)
  /** Tag this slot.
    * @param up Some(true) for up, Some(false) for down, or None to remove
    * @return true if the tag name is valid
    */
  def setTag(tag : String, up : Option[Boolean] = Some(true))(implicit site : AuthSite) : Boolean =
    Tag.valid(tag).fold(false) { n =>
      Tag.getOrCreate(n).map(_.set(this, up))
      true
    }

  /** The list of records on this object.
    * @param all include indirect records on any contained objects
    */
  def records : Future[Seq[Record]] = Record.getSlot(this)
  /** Remove the given record from this slot. */
  def removeRecord(rec : Record.Id) : Unit = Record.removeSlot(rec, id)
  /** The list of records and possibly measures on this object.
    * This is essentially equivalent to `this.records(false).filter(_.category == category).map(r => (r, r.measure[T](metric)))` but more efficient.
    * @param category if Some limit to the given category */
  private def recordMeasures[T](category : Option[RecordCategory] = None, metric : Metric[T] = Metric.Ident) : Future[Seq[(Record, Option[T])]] =
    MeasureV.getSlot[T](this, category, metric)
  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  private val idents : Future[Seq[String]] =
    recordMeasures[String]() map {
      groupBy(_, (ri : (Record,Option[String])) => ri._1.category).map { case (c,l) =>
        c.fold("")(_.name.capitalize + " ") + l.map { case (r,i) => i.getOrElse("[" + r.id.toString + "]") }.mkString(", ")
      }
    }

  /** An image-able "asset" that may be used as the slot's thumbnail. */
  def thumb : Future[Option[SlotAsset]] = SlotAsset.getThumb(this)

  def pageName = container.name.getOrElse { 
    val i = Async.get(idents)
    if (i.isEmpty)
      if (container.top)
        volume.name
      else
        "Session [" + id + "]"
    else
      "Session: " + i.mkString(", ")
  }
  override def pageCrumbName = if (segment.isFull) None else Some(segment.lowerBound.fold("")(_.toString) + " - " + segment.upperBound.fold("")(_.toString))
  def pageParent = Some(if (isContext) volume else Async.get(context))
  def pageURL = controllers.routes.Slot.view(container.volumeId, id)
  def pageActions = Seq(
    Action("view", controllers.routes.Slot.view(volumeId, id), Permission.VIEW),
    Action("edit", controllers.routes.Slot.edit(volumeId, id), Permission.EDIT),
    Action("add file", controllers.routes.Asset.create(volumeId, containerId, segment.lowerBound), Permission.CONTRIBUTE),
    // Action("add slot", controllers.routes.Slot.create(volumeId, containerId), Permission.CONTRIBUTE),
    Action("add participant", controllers.routes.Record.slotAdd(volumeId, id, IntId[models.RecordCategory](-500), false), Permission.CONTRIBUTE)
  )
}

object Slot extends TableId[Slot]("slot") {
  private[models] def make(container : Container)(id : Id, segment : Range[Offset], consent : Consent.Value) =
    new Slot(id, container, segment, consent)
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Range[Offset]]("segment")
    , SelectColumn[Consent.Value]("consent")
    )
  private[models] def row(implicit site : Site) =
    columns.join(Container.row, "slot.source = container.id") map {
      case (slot, cont) => (make(cont) _).tupled(slot)
    }
  private[models] def containerRow(container : Container) =
    columns map (make(container) _).tupled
  private[models] def volumeRow(volume : Volume) =
    columns.join(Container.volumeRow(volume), "slot.source = container.id") map {
      case (slot, cont) => (make(cont) _).tupled(slot)
    }

  final val fullRange = Range.full[Offset]

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Slot]] =
    row.SELECT("WHERE slot.id = ? AND", Volume.condition)
      .apply(i +: Volume.conditionArgs).singleOpt

  private def _get(container : Container, segment : Range[Offset])(implicit dbc : Site.DB, exc : ExecutionContext) : Future[Option[Slot]] =
    containerRow(container).SELECT("WHERE slot.source = ? AND slot.segment = ?")(dbc, exc)
      .apply(container.id, segment).singleOpt

  /** Retrieve an individual Slot by Container and segment.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  private[models] def get(container : Container, segment : Range[Offset]) : Future[Option[Slot]] =
    if (segment.isFull) container.fullSlot.map(Some(_)) else // optimization
    _get(container, segment)

  /** Retrieve a list of slots within the given container. */
  private[models] def getContainer(c : Container) : Future[Seq[Slot]] =
    containerRow(c).SELECT("WHERE slot.source = ? ORDER BY slot.segment")
      .apply(c.id).list

  /** Retrieve the master slot for a volume. */
  private[models] def getTop(v : Volume) : Future[Slot] =
    volumeRow(v).SELECT("WHERE slot.segment = '(,)' AND container.volume = ? AND container.top")
      .apply(v.id).single

  /** Create a new slot in the specified container or return a matching one if it already exists. */
  def getOrCreate(container : Container, segment : Range[Offset]) : Future[Slot] =
    if (segment.isFull) container.fullSlot else // optimization
    DBUtil.selectOrInsert(_get(container, segment)(_, _)) { (dbc, exc) =>
      val args = SQLTerms('source -> container.id, 'segment -> segment)
      SQL("INSERT INTO slot " + args.insert + " RETURNING id")(dbc, exc)
        .apply(args).single(SQLCols[Id].map(new Slot(_, container, segment)))
    }
}
