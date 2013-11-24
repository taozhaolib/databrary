package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

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

  if (isFull)
    container._fullSlot = this

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

  /* must be set on construction */
  private var _context : Slot = this
  /** The relevant consented slot containing this one.
    * Defaults to fullSlot. */
  def context = _context

  def getConsent : Consent.Value = _context.consent

  private val publicFields = Array(org.joda.time.DateTimeFieldType.year)
  def getDate : Option[org.joda.time.ReadablePartial] =
    container.date.map { date =>
      if (Permission.data(getPermission, getConsent, Classification.IDENTIFIED).checkPermission(Permission.DOWNLOAD))
        date
      else
        new org.joda.time.Partial(publicFields, publicFields.map(date.get _))
    }

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
  def postComment(text : String, parent : Option[Comment.Id] = None)(implicit site : AuthSite) : Future[Boolean] =
    Comment.post(this, text, parent)

  /** The list of tags on the current slot along with the current user's applications.
    * @param all add any tags applied to child slots to weight (but not use) as well */
  def tags(all : Boolean = true) = TagWeight.getSlot(this, all)
  /** Tag this slot.
    * @param up Some(true) for up, Some(false) for down, or None to remove
    * @return true if the tag name is valid
    */
  def setTag(tag : String, up : Option[Boolean] = Some(true))(implicit site : AuthSite) : Future[Boolean] =
    Tag.valid(tag).fold(Async(false))(
      Tag.getOrCreate(_).flatMap(_.set(this, up)))

  private val _records : FutureVar[Seq[Record]] = FutureVar[Seq[Record]](Record.getSlot(this))
  /** The list of records on this object.
    * @param all include indirect records on any contained objects
    */
  def records : Future[Seq[Record]] = _records.apply
  /** Remove the given record from this slot. */
  def removeRecord(rec : Record.Id) : Future[Boolean] = Record.removeSlot(rec, id)
  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  private def idents : Seq[String] =
    _records.peek.fold[Seq[String]](Nil) {
      groupBy[Record,Option[RecordCategory]](_, ri => ri.category)
      .map { case (c,l) =>
        c.fold("")(_.name.capitalize + " ") + l.map(_.ident).mkString(", ")
      }
    }

  /** An image-able "asset" that may be used as the slot's thumbnail. */
  def thumb : Future[Option[SlotAsset]] = SlotAsset.getThumb(this)

  def pageName = container.name.getOrElse { 
    val i = idents
    if (i.isEmpty)
      if (container.top)
        volume.name
      else
        "Session [" + id + "]"
    else
      "Session: " + i.mkString(", ")
  }
  override def pageCrumbName : Option[String] = if (isFull) None else Some(segment.lowerBound.fold("")(_.toString) + " - " + segment.upperBound.fold("")(_.toString))
  def pageParent = Some(if (isContext) volume else context)
  def pageURL = controllers.routes.Slot.view(container.volumeId, id)
  def pageActions = Seq(
    Action("view", controllers.routes.Slot.view(volumeId, id), Permission.VIEW),
    Action("edit", controllers.routes.Slot.edit(volumeId, id), Permission.EDIT),
    Action("add file", controllers.routes.Asset.create(volumeId, id), Permission.CONTRIBUTE),
    // Action("add slot", controllers.routes.Slot.create(volumeId, containerId), Permission.CONTRIBUTE),
    Action("add participant", controllers.routes.Record.slotAdd(volumeId, id, IntId[models.RecordCategory](-500), false), Permission.CONTRIBUTE)
  )
}

object Slot extends TableId[Slot]("slot") {
  private val base = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Range[Offset]]("segment")
    , SelectColumn[Consent.Value]("consent")
    ).map { (id, segment, consent) =>
      (container : Container) =>
        new Slot(id, container, segment, consent)
    }
  private[models] val columns = base
    .leftJoin(base.fromAlias("context"), "slot.source = context.source AND slot.segment <@ context.segment AND context.consent IS NOT NULL")
    .map { case (slot, context) =>
      (container : Container) =>
        val s = slot(container)
        s._context = 
          if (s.consent != Consent.NONE) s
          else if (container.fullSlot.consent != Consent.NONE) container.fullSlot
          else context.map(_(container)).getOrElse(container.fullSlot)
        s
    }
  private[models] def row(implicit site : Site) =
    columns.join(Container.row(false), "slot.source = container.id") map {
      case (slot, cont) => slot(cont)
    }
  private[models] def containerRow(container : Container) =
    columns.map(_(container))
  private[models] def volumeRow(volume : Volume, full : Boolean) =
    columns.join(Container.volumeRow(volume, full), "slot.source = container.id") map {
      case (slot, cont) => slot(cont)
    }

  object Full {
    final val range : Range[Offset] = Range.full[Offset]

    private[models] val columns = Columns(
        SelectColumn[Slot.Id]("id")
      , SelectColumn[Consent.Value]("consent")
      ).map { (id, consent) =>
        (container : Container) =>
          new Slot(id, container, range, consent)
      }
    private[models] def row(implicit site : Site) =
      columns.join(Container.row(true), "slot.source = container.id AND slot.segment = '(,)')") map {
        case (slot, cont) => slot(cont)
      }
    private[models] def containerRow(container : Container) =
      columns.map(_(container))

    /** Retrieve an individual Slot.
      * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access.
      * @param full only return full slots */
    def get(i : Slot.Id)(implicit site : Site) : Future[Option[Slot]] =
      row.SELECT("WHERE slot.id = ? AND", Volume.condition)
        .apply(i +: Volume.conditionArgs).singleOpt
  }

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access.
    * @param full only return full slots */
  def get(i : Slot.Id)(implicit site : Site) : Future[Option[Slot]] =
    row.SELECT("WHERE slot.id = ? AND", Volume.condition)
      .apply(i +: Volume.conditionArgs).singleOpt

  private def _get(container : Container, segment : Range[Offset])(implicit dbc : Site.DB, exc : ExecutionContext) : Future[Option[Slot]] =
    containerRow(container).SELECT("WHERE slot.source = ? AND slot.segment = ?")(dbc, exc)
      .apply(container.id, segment).singleOpt

  /** Retrieve an individual Slot by Container and segment.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  private[models] def get(container : Container, segment : Range[Offset]) : Future[Option[Slot]] =
    if (segment.isFull) Async(Some(container.fullSlot)) else
    _get(container, segment)

  /** Retrieve a list of slots within the given container. */
  private[models] def getContainer(c : Container) : Future[Seq[Slot]] =
    containerRow(c).SELECT("WHERE slot.source = ? ORDER BY slot.segment")
      .apply(c.id).list

  /** Create a new slot in the specified container or return a matching one if it already exists. */
  def getOrCreate(container : Container, segment : Range[Offset]) : Future[Slot] =
    if (segment.isFull) Async(container.fullSlot) else
    DBUtil.selectOrInsert(_get(container, segment)(_, _)) { (dbc, exc) =>
      val args = SQLTerms('source -> container.id, 'segment -> segment)
      SQL("INSERT INTO slot " + args.insert + " RETURNING id")(dbc, exc)
        .apply(args).single(SQLCols[Id].map(new Slot(_, container, segment)))
    }
}
