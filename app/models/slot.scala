package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import dbrary._
import site._

/** Conceptually a slot represents a segment of a container. */
trait AbstractSlot extends InVolume with SiteObject {
  def container : Container
  val segment : Range[Offset]

  final def containerId : Container.Id = container.id
  def volume = container.volume

  /** True if this is its container's full slot. */
  def isFull : Boolean = segment.isFull
  def isTop : Boolean = container.top && isFull
  /** Effective start point of this slot within the container. */
  final def position : Offset = segment.lowerBound.getOrElse(Offset.ZERO)

  /** Turn this abstract slot into a real slot in the database. */
  def realize : Future[Slot]
  private[models] def sqlId : String
  private[models] def sqlArgs : SQLArgs

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

  /** List of asset that overlap with this slot. */
  def assets : Future[Seq[SlotAsset]] = SlotAsset.getSlotAll(this)

  /** The list of records that apply to this slot. */
  def records = Record.getSlotAll(this)

  /** The list of comments that apply to this slot. */
  final def comments = Comment.getSlotAll(this)
  /** Post a new comment this object. */
  final def postComment(text : String, parent : Option[Comment.Id] = None)(implicit site : AuthSite) : Future[Boolean] =
    Comment.post(this, text, parent)

  /** The list of tags on the current slot along with the current user's applications. */
  final def tags = TagWeight.getSlotAll(this)
  /** Tag this slot.
    * @param up Some(true) for up, Some(false) for down, or None to remove
    * @return true if the tag name is valid
    */
  final def setTag(tag : String, up : Option[Boolean] = Some(true))(implicit site : AuthSite) : Future[Boolean] =
    Tag.valid(tag).fold(Async(false))(tname => for {
      t <- Tag.getOrCreate(tname)
      s <- realize
      r <- t.set(s, up)
    } yield(r))

  def pageName = container.name.getOrElse("Slot")
  override def pageCrumbName : Option[String] = if (isFull) None else Some(segment.lowerBound.fold("")(_.toString) + "-" + segment.upperBound.fold("")(_.toString))
  def pageParent : Option[SitePage] = Some(context)
  def pageURL = controllers.routes.SlotHtml.view(containerId, segment.lowerBound, segment.upperBound)
  def pageActions = Seq(
    Action("view", pageURL, Permission.VIEW)
  )

  def json : JsonObject = JsonObject(
    'container -> container.json,
    'segment -> segment
    // Maybe(getConsent).opt.map('consent -> _)
  )

  def json(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(json.obj, options,
      "assets" -> (opt => SlotAsset.getSlotAll(this).map(JsonArray.map(_.inContext.json - "container"))),
      "records" -> (opt => records.map(JsonRecord.map { r =>
        r.json ++ JsonObject.flatten(r.age(this).map('age -> _))
      })),
      "tags" -> (opt => tags.map(JsonRecord.map(_.json))),
      "comments" -> (opt => comments.map(JsonRecord.map(_.json - "container")))
    )
}

/** Smallest organizatonal unit of related data.
  * Primarily used for an individual session of data with a single date and place.
  * Critically, contained data are should be covered by a single consent level and share the same annotations. */
abstract class Slot protected (val id : Slot.Id, val segment : Range[Offset], consent_ : Consent.Value = Consent.NONE) extends TableRowId[Slot] with AbstractSlot {
  private[this] final var _consent = consent_
  /** Directly assigned consent for covered assets. */
  final def consent = _consent

  final override def realize = Async(this)
  private[models] def sqlId : String = "?"
  private[models] def sqlArgs : SQLArgs = SQLArgs(id)

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
  override final def assets : Future[Seq[SlotAsset]] = SlotAsset.getSlot(this)

  /** The list of tags on the current slot along with the current user's applications.
    * @param all add any tags applied to child slots to weight (but not use) as well, and if this is the top slot, return all volume tags instead */
  final def tags(all : Boolean = true) =
    if (all) super.tags
    else TagWeight.getSlot(this)

  private[this] final val _records : FutureVar[Seq[Record]] = FutureVar[Seq[Record]](Record.getSlot(this))
  /** The list of records on this object.
    */
  override final def records : Future[Seq[Record]] = _records.apply
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

  override def pageName = container.name.getOrElse { 
    val i = idents
    if (i.isEmpty)
      if (container.top)
        volume.name
      else
        "Session [" + id + "]"
    else
      "Session: " + i.mkString(", ")
  }
  override def pageParent = Some(if (isContext) volume else context)
  override def pageURL = controllers.routes.SlotHtml.view(id)
  override def pageActions = super.pageActions ++ Seq(
    Action("edit", controllers.routes.SlotHtml.edit(volumeId, id), Permission.EDIT),
    Action("add file", controllers.Asset.routes.html.create(volumeId, id), Permission.CONTRIBUTE),
    // Action("add slot", controllers.routes.Slot.create(volumeId, containerId), Permission.CONTRIBUTE),
    Action("add participant", controllers.routes.RecordHtml.slotAdd(volumeId, id, RecordCategory.PARTICIPANT, false), Permission.CONTRIBUTE)
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
  /** A slot that is not (necessarily) in the database, but for which we know all the relevant information. */
  private final class VirtualSlot (val segment : Range[Offset], val context : Slot) extends AbstractSlot {
    def container = context.container
    def realize = Slot.getOrCreate(container, segment)
    private[models] def sqlId : String = "get_slot(?, ?::segment)"
    private[models] def sqlArgs : SQLArgs = SQLArgs(containerId, segment)
  }

  private val base = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Range[Offset]]("segment")
    , SelectColumn[Consent.Value]("consent")
    ).map { (id, segment, consent) =>
      (container : Container) =>
        if ((id === container.id).ensuring(_ == segment.isFull))
          container.ensuring(_.consent == consent)
        else
          new SubSlot(id, container, segment, consent)
    }
  private val context = base.fromAlias("context")
  private[models] val columns : Selector[Container => Slot] = base
    .leftJoin(context, "slot.source = context.source AND slot.segment <@ context.segment AND context.consent IS NOT NULL")
    .map {
      case (slot, None) =>
        (container : Container) => slot(container)
      case (slot, Some(context)) =>
        (container : Container) =>
          val s = slot(container)
          s match {
            case s : SubSlot if !(s.hasConsent || container.hasConsent) =>
              s._context = context(container)
            case _ =>
          }
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

  def get(id : Slot.Id, segment : Range[Offset])(implicit site : Site) : Future[Option[AbstractSlot]] =
    if (segment.isFull) /* may be container or actual slot id */
      get(id)
    else /* must be container id */ Container.row
      .leftJoin(base, "container.id = slot.source AND slot.segment = ?::segment")
      .leftJoin(context, "container.id = context.source AND ?::segment <@ context.segment AND context.consent IS NOT NULL")
      .map {
        case ((c, Some(slot)), _) => slot(c)
        case ((c, None), None) => new VirtualSlot(segment, c)
        case ((c, None), Some(slot)) => new VirtualSlot(segment, slot(c))
      }
      .SELECT("WHERE container.id = ? AND", Volume.condition)
      .apply(SQLArgs(segment, segment, id) ++ Volume.conditionArgs).singleOpt

  /** Retrieve a list of slots within the given container. */
  private[models] def getContainer(c : Container) : Future[Seq[Slot]] =
    containerRow(c).SELECT("WHERE slot.source = ? ORDER BY slot.segment")
      .apply(c.id).list

  private def _get(container : Container, segment : Range[Offset])(implicit dbc : Site.DB, exc : ExecutionContext) : Future[Option[Slot]] =
    containerRow(container).SELECT("WHERE slot.source = ? AND slot.segment = ?::segment")(dbc, exc)
      .apply(container.id, segment).singleOpt

  /** Create a new slot in the specified container or return a matching one if it already exists. */
  def getOrCreate(container : Container, segment : Range[Offset]) : Future[Slot] =
    if (segment.isFull) Async(container) else
    DBUtil.selectOrInsert(_get(container, segment)(_, _)) { (dbc, exc) =>
      val args = SQLTerms('source -> container.id, 'segment -> segment)
      SQL("INSERT INTO slot", args.insert, "RETURNING id")(dbc, exc)
        .apply(args).single(SQLCols[Id].map(new SubSlot(_, container, segment)))
    }
}
