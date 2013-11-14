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

  def getConsent : Consent.Value = Async.wait(context).consent

  /** The level of access granted on data covered by this slot to the current user. */
  def dataPermission(classification : Classification.Value = Classification.RESTRICTED) : HasPermission =
    Permission.data(volume.permission, getConsent, classification)

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

  /** The list of records on this object.
    * @param all include indirect records on any contained objects
    */
  lazy val records : Future[Seq[Record]] = Record.getSlot(this)
  /** Remove the given record from this slot. */
  def removeRecord(rec : Record.Id) : Future[Boolean] = Record.removeSlot(rec, id)
  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  private lazy val idents : Future[Seq[String]] =
    records.map {
      groupBy[Record,Option[RecordCategory]](_, ri => ri.category)
      .map { case (c,l) =>
        c.fold("")(_.name.capitalize + " ") + l.map(_.ident).mkString(", ")
      }
    }

  /** An image-able "asset" that may be used as the slot's thumbnail. */
  def thumb : Future[Option[SlotAsset]] = SlotAsset.getThumb(this)

  def pageName = container.name.getOrElse { 
    val i = Async.wait(idents)
    if (i.isEmpty)
      if (container.top)
        volume.name
      else
        "Session [" + id + "]"
    else
      "Session: " + i.mkString(", ")
  }
  override def pageCrumbName = if (segment.isFull) None else Some(segment.lowerBound.fold("")(_.toString) + " - " + segment.upperBound.fold("")(_.toString))
  def pageParent = Some(if (isContext) volume else Async.wait(context))
  def pageURL = controllers.routes.Slot.view(container.volumeId, id)
  def pageActions = Seq(
    Action("view", controllers.routes.Slot.view(volumeId, id), Permission.VIEW),
    Action("edit", controllers.routes.Slot.edit(volumeId, id), Permission.EDIT),
    Action("add file", controllers.routes.Asset.create(volumeId, id, segment.lowerBound), Permission.CONTRIBUTE),
    // Action("add slot", controllers.routes.Slot.create(volumeId, containerId), Permission.CONTRIBUTE),
    Action("add participant", controllers.routes.Record.slotAdd(volumeId, id, IntId[models.RecordCategory](-500), false), Permission.CONTRIBUTE)
  )
}

object Slot extends TableId[Slot]("slot") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Range[Offset]]("segment")
    , SelectColumn[Consent.Value]("consent")
    ).map { (id, segment, consent) =>
      (container : Container) => new Slot(id, container, segment, consent)
    }
  private[models] def row(implicit site : Site) =
    columns.join(Container.row, "slot.source = container.id") map {
      case (slot, cont) => slot(cont)
    }
  private[models] def containerRow(container : Container) =
    columns.map(_(container))
  private[models] def volumeRow(volume : Volume) =
    columns.join(Container.volumeRow(volume), "slot.source = container.id") map {
      case (slot, cont) => slot(cont)
    }

  final val fullRange = Range.full[Offset]

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access.
    * @param full only return full slots */
  def get(i : Id, full : Boolean = false)(implicit site : Site) : Future[Option[Slot]] =
    row.SELECT("WHERE slot.id = ?",
      if (full) "AND slot.segment = '(,)'" else "",
      "AND", Volume.condition)
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
