package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import dbrary._
import site._

/** Conceptually a slot represents a segment of a container. */
trait AbstractSlot extends InVolume {
  def container : Container
  val segment : Range[Offset]

  final def containerId : Container.Id = container.id
  def volume = container.volume

  /** True if this is its container's full slot. */
  def isFull : Boolean = segment.isFull
  def isTop : Boolean = container.top && isFull
  /** Effective start point of this slot within the container. */
  final def position : Offset = segment.lowerBound.getOrElse(0)

  /** The relevant consented slot containing this one. */
  def context : Slot
  /** The effective consent level that applies to contained data. */
  final def getConsent : Consent.Value = context.consent

  /** The permisison level granted to identifiable data within this slot. */
  final def dataPermission : HasPermission =
    Permission.data(getPermission, getConsent, Classification.IDENTIFIED)
  /** Whether the current user may download identifiable data within this slot. */
  final lazy val downloadable : Boolean =
    dataPermission.checkPermission(Permission.DOWNLOAD)

  private[this] final val publicDateFields = Array(org.joda.time.DateTimeFieldType.year)
  final def getDate : Option[org.joda.time.ReadablePartial] =
    container.date.map { date =>
      if (downloadable) date
      else new org.joda.time.Partial(publicDateFields, publicDateFields.map(date.get _))
    }

  /** The list of comments that apply to this object. */
  final def comments = Comment.getSlotAll(this)

  /** The list of tags on the current slot along with the current user's applications. */
  final def tags =
    if (isTop)
      TagWeight.getVolume(volume)
    else
      TagWeight.getSlotAll(this)

  lazy val jsonFields = JsonObject.flatten(
    Some('container -> container.json),
    Some('segment -> segment)
    // Maybe(getConsent).opt.map('consent -> _)
  )

  def json(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(jsonFields, options,
      "assets" -> (opt => SlotAsset.getSlotAll(this).map(JsonArray.map(_.inContext.json - "container"))),
      "records" -> (opt => Record.getSlotAll(this).map(JsonRecord.map(_.json))),
      "tags" -> (opt => tags.map(JsonRecord.map(_.json))),
      "comments" -> (opt => comments.map(JsonRecord.map(_.json)))
    )
}

/** Smallest organizatonal unit of related data.
  * Primarily used for an individual session of data with a single date and place.
  * Critically, contained data are should be covered by a single consent level and share the same annotations. */
abstract class Slot protected (val id : Slot.Id, val segment : Range[Offset], consent_ : Consent.Value = Consent.NONE) extends TableRowId[Slot] with AbstractSlot with SiteObject {
  private[this] final var _consent = consent_
  /** Directly assigned consent for covered assetsd. */
  final def consent = _consent

  final def hasConsent : Boolean = consent != Consent.NONE
  final def isContext : Boolean = hasConsent || isFull

  /** Update the given values in the database and this object in-place. */
  final def setConsent(consent : Consent.Value = _consent) : Future[Boolean] = {
    if (consent == _consent)
      return Async(true)
    Audit.change("slot", SQLTerms('consent -> Maybe(consent).opt), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        _consent = consent
      }
  }

  /** List of contained asset segments within this slot. */
  final def assets : Future[Seq[SlotAsset]] = SlotAsset.getSlot(this)

  /** Post a new comment this object.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  final def postComment(text : String, parent : Option[Comment.Id] = None)(implicit site : AuthSite) : Future[Boolean] =
    Comment.post(this, text, parent)

  /** The list of tags on the current slot along with the current user's applications.
    * @param all add any tags applied to child slots to weight (but not use) as well, and if this is the top slot, return all volume tags instead */
  final def tags(all : Boolean = true) =
    if (all) super.tags
    else TagWeight.getSlot(this)
  /** Tag this slot.
    * @param up Some(true) for up, Some(false) for down, or None to remove
    * @return true if the tag name is valid
    */
  final def setTag(tag : String, up : Option[Boolean] = Some(true))(implicit site : AuthSite) : Future[Boolean] =
    Tag.valid(tag).fold(Async(false))(
      Tag.getOrCreate(_).flatMap(_.set(this, up)))

  private[this] final val _records : FutureVar[Seq[Record]] = FutureVar[Seq[Record]](Record.getSlot(this))
  /** The list of records on this object.
    */
  final def records : Future[Seq[Record]] = _records.apply
  /** Remove the given record from this slot. */
  final def removeRecord(rec : Record.Id) : Future[Boolean] = Record.removeSlot(rec, id)
  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  private final def idents : Seq[String] =
    _records.peek.fold[Seq[String]](Nil) {
      groupBy[Record,Option[RecordCategory]](_, ri => ri.category)
      .map { case (c,l) =>
        c.fold("")(_.name.capitalize + " ") + l.map(_.ident).mkString(", ")
      }
    }

  /** An image-able "asset" that may be used as the slot's thumbnail. */
  final def thumb : Future[Option[SlotAsset]] = SlotAsset.getThumb(this)

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
  def pageURL = controllers.Slot.routes.html.view(container.volumeId, id)
  def pageActions = Seq(
    Action("view", pageURL, Permission.VIEW),
    Action("edit", controllers.Slot.routes.html.edit(volumeId, id), Permission.EDIT),
    Action("add file", controllers.Asset.routes.html.create(volumeId, id), Permission.CONTRIBUTE),
    // Action("add slot", controllers.routes.Slot.create(volumeId, containerId), Permission.CONTRIBUTE),
    Action("add participant", controllers.Record.routes.html.slotAdd(volumeId, id, RecordCategory.PARTICIPANT, false), Permission.CONTRIBUTE)
  )
}

object Slot extends TableId[Slot]("slot") {
  private final class SubSlot (id : Slot.Id, val container : Container, segment : Range[Offset], consent_ : Consent.Value = Consent.NONE) extends Slot(id, segment, consent_) {
    /* must be set on construction */
    private[Slot] var _context : Slot = if (isContext) this else container
    /** The relevant consented slot containing this one.
      * Defaults to fullSlot. */
    def context = _context
  }

  private val base = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Range[Offset]]("segment")
    , SelectColumn[Consent.Value]("consent")
    ).map { (id, segment, consent) =>
      (container : Container) =>
        new SubSlot(id, container, segment, consent)
    }
  private val context = base.fromAlias("context")
  private[models] val columns : Selector[Container => Slot] = base
    .leftJoin(context, "slot.source = context.source AND slot.segment <@ context.segment AND context.consent IS NOT NULL")
    .map { case (slot, context) =>
      (container : Container) =>
        val s = slot(container)
        if (!(s.hasConsent || container.hasConsent))
          s._context = context.fold[Slot](container)(_(container))
        s
    }
  private[models] def row(implicit site : Site) =
    columns.join(Container.row, "slot.source = container.id") map {
      case (slot, cont) => slot(cont)
    }
  private[models] def containerRow(container : Container) =
    (if (container.hasConsent)
      base
    else
      columns)
      .map(_(container))
  private[models] def volumeRow(volume : Volume) =
    columns.join(Container.volumeRow(volume), "slot.source = container.id") map {
      case (slot, cont) => slot(cont)
    }

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access.
    * @param full only return full slots */
  def get(i : Slot.Id)(implicit site : Site) : Future[Option[Slot]] =
    row.SELECT("WHERE slot.id = ? AND", Volume.condition)
      .apply(i +: Volume.conditionArgs).singleOpt

  private def _get(container : Container, segment : Range[Offset])(implicit dbc : Site.DB, exc : ExecutionContext) : Future[Option[Slot]] =
    containerRow(container).SELECT("WHERE slot.source = ? AND slot.segment = ?::segment")(dbc, exc)
      .apply(container.id, segment).singleOpt

  /** Retrieve an individual Slot by Container and segment.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  private[models] def get(container : Container, segment : Range[Offset]) : Future[Option[Slot]] =
    if (segment.isFull) Async(Some(container)) else
    _get(container, segment)

  /** Retrieve a list of slots within the given container. */
  private[models] def getContainer(c : Container) : Future[Seq[Slot]] =
    containerRow(c).SELECT("WHERE slot.source = ? ORDER BY slot.segment")
      .apply(c.id).list

  /** Create a new slot in the specified container or return a matching one if it already exists. */
  def getOrCreate(container : Container, segment : Range[Offset]) : Future[Slot] =
    if (segment.isFull) Async(container) else
    DBUtil.selectOrInsert(_get(container, segment)(_, _)) { (dbc, exc) =>
      val args = SQLTerms('source -> container.id, 'segment -> segment)
      SQL("INSERT INTO slot", args.insert, "RETURNING id")(dbc, exc)
        .apply(args).single(SQLCols[Id].map(new SubSlot(_, container, segment)))
    }

  object Virtual {
    /** A slot that is not (necessarily) in the database, but for which we know all the relevant information. */
    private final class VirtualSlot (val segment : Range[Offset], val context : Slot) extends AbstractSlot {
      def container = context.container
    }

    private def make(segment : Range[Offset], context : Slot) : AbstractSlot =
      if (context.segment.equals(segment)) context
      else new VirtualSlot(segment, context)

    private def columns(segment : Range[Offset]) = context.map { context =>
      (container : Container) =>
        make(segment, context(container))
    }

    def get(container : Container, segment : Range[Offset]) : Future[AbstractSlot] =
      if (segment.isFull) Async(container)
      else if (container.hasConsent) Async(new VirtualSlot(segment, container))
      else columns(segment).map(_(container))
        .SELECT("WHERE context.source = ? AND context.segment @> ?::segment AND context.consent IS NOT NULL")
        .apply(container.id, segment).singleOpt
        .map(_.getOrElse(container))

    def get(container : Container.Id, segment : Range[Offset])(implicit site : Site) : Future[Option[AbstractSlot]] =
      Container.row
        .leftJoin(context, "container.id = context.source AND context.segment @> ?::segment AND context.consent IS NOT NULL")
        .map { case (cont, slot) => make(segment, slot.fold[Slot](cont)(_(cont))) }
        .SELECT("WHERE container.id = ? AND", Volume.condition)
        .apply(SQLArgs(segment, container) ++ Volume.conditionArgs).singleOpt
  }
}
