package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import dbrary._
import site._

/** Conceptually a slot represents a segment of a container. */
trait Slot extends TableRow with InVolume with SiteObject {
  def container : Container = context.container
  val segment : Segment
  def context : ContextSlot
  /** The effective consent level that applies to contained data. */
  def consent : Consent.Value = context.consent

  final def containerId : Container.Id = container.id
  final def volume = container.volume

  /** True if this is its container's full slot. */
  final def isFull : Boolean = segment.isFull
  final def isTop : Boolean = container.top && isFull
  /** Effective start point of this slot within the container. */
  final def position : Offset = segment.lowerBound.getOrElse(Offset.ZERO)

  private[models] final def sql : SQLTerms = SQLTerms('container -> containerId, 'segment -> segment)

  /** Update the given values in the database and this object in-place. */
  final def setConsent(consent : Consent.Value) : Future[Boolean] = {
    if (consent == Consent.NONE)
      Audit.remove("slot_consent", sql).execute
    else
      Audit.changeOrAdd("slot_consent", SQLTerms('consent -> consent), sql).execute
	.recover {
	  case e : com.github.mauricio.async.db.postgresql.exceptions.GenericDatabaseException if e.errorMessage.message.startsWith("conflicting key value violates exclusion constraint ") => false
	}
  }

  /** The permisison level granted to identifiable data within this slot. */
  final def dataPermission : HasPermission =
    Permission.data(permission, consent, Classification.IDENTIFIED)
  /** Whether the current user may download identifiable data within this slot. */
  final lazy val downloadable : Boolean =
    dataPermission.checkPermission(Permission.DOWNLOAD)

  final def getDate : Option[org.joda.time.ReadablePartial] =
    container.date.map { date =>
      if (downloadable) date
      else new org.joda.time.Partial(Permission.publicDateFields, Permission.publicDateFields.map(date.get _))
    }

  /** List of asset that overlap with this slot. */
  final def assets : Future[Seq[SlotAsset]] = SlotAsset.getSlot(this)

  /** An image-able "asset" that may be used as the slot's thumbnail. */
  final def thumb : Future[Option[SlotAsset]] = SlotAsset.getThumb(this)

  private[this] def _records : FutureVar[Seq[Record]] = FutureVar[Seq[Record]](Record.getSlot(this))
  /** The list of records that apply to this slot. */
  final def records : Future[Seq[Record]] = _records.apply

  /** The list of comments that apply to this slot. */
  final def comments = Comment.getSlot(this)
  /** Post a new comment this object. */
  final def postComment(text : String, parent : Option[Comment.Id] = None)(implicit site : AuthSite) : Future[Boolean] =
    Comment.post(this, text, parent)

  /** The list of tags on the current slot along with the current user's applications. */
  final def tags = TagWeight.getSlot(this)
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

  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  private[this] def idents : Seq[String] =
    _records.peek.fold[Seq[String]](Nil) {
      groupBy[Record,Option[RecordCategory]](_, ri => ri.category)
      .map { case (c,l) =>
        c.fold("")(_.name.capitalize + " ") + l.map(_.ident).mkString(", ")
      }
    }

  def pageName = container.name.getOrElse { 
    val i = idents
    if (i.isEmpty)
      if (container.top)
        volume.name
      else
        "Slot"
    else
      i.mkString(", ")
  }
  override def pageCrumbName : Option[String] = if (isFull) None else Some(segment.lowerBound.fold("")(_.toString) + "-" + segment.upperBound.fold("")(_.toString))
  def pageParent : Option[SitePage] = Some(container)
  def pageURL = controllers.routes.SlotHtml.view(containerId, segment)
  def pageActions = Seq(
    Action("view", pageURL, Permission.VIEW),
    Action("edit", controllers.routes.SlotHtml.edit(containerId, segment), Permission.EDIT),
    Action("add file", controllers.routes.AssetHtml.create(volumeId, Some(containerId), segment.lowerBound), Permission.CONTRIBUTE),
    Action("add participant", controllers.routes.RecordHtml.slotAdd(containerId, segment, RecordCategory.PARTICIPANT, false), Permission.CONTRIBUTE)
  )

  lazy val json : JsonObject = JsonObject.flatten(
    Some('container -> container.containerJson),
    if (segment.isFull) None else Some('segment -> segment)
    // Maybe(getConsent).opt.map('consent -> _)
  )

  def json(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(json.obj, options,
      "assets" -> (opt => assets.map(JsonArray.map(_.inContext.json - "container"))),
      "records" -> (opt => records.map(JsonRecord.map { r =>
        r.json ++ JsonObject.flatten(r.age(this).map('age -> _))
      })),
      "tags" -> (opt => tags.map(JsonRecord.map(_.json))),
      "comments" -> (opt => comments.map(JsonRecord.map(_.json - "container")))
    )
}

trait ContextSlot extends Slot {
  override val container : Container
  override def context = this
  override val consent : Consent.Value

  override def pageParent = Some(volume)
}

final class SlotConsent private (val container : Container, val segment : Segment, val consent : Consent.Value) extends ContextSlot

private[models] class SlotTable[R <: Slot](table : String) extends Table[R](table) {
  protected val segment = SelectColumn[Segment]("segment")
}

object SlotConsent extends SlotTable[SlotConsent]("slot_consent") {
  private val columns = Columns(
    segment,
    SelectColumn[Consent.Value]("consent")
  )
  private[models] val row = columns
    .map { (segment, consent) =>
      (container : Container) => new SlotConsent(container, segment, consent)
    }
}

object Slot extends SlotTable[Slot]("slot") {
  private final class VirtualSlot private (val segment : Segment, val context : ContextSlot) extends Slot
  private object VirtualSlot {
    def apply(segment : Segment, context : ContextSlot) : Slot =
      if (segment === context.segment) context
      else new VirtualSlot(segment, context)
  }
  private def make(segment : Segment)(container : Container, consent : Option[Container => SlotConsent]) : Slot =
    VirtualSlot(segment, consent.fold[ContextSlot](container)(_(container)))

  def get(containerId : Container.Id, segment : Segment)(implicit site : Site) : Future[Option[Slot]] =
    Container.row
      .leftJoin(SlotConsent.row, "container.id = slot_consent.container AND slot_consent.segment @> ?::segment")
      .map((make(segment) _).tupled)
      .SELECT("WHERE container.id = ? AND", Volume.condition)
      .apply(SQLArgs(segment, containerId) ++ Volume.conditionArgs).singleOpt

  def get(container : Container, segment : Segment) : Future[Slot] =
    SlotConsent.row.SELECT("WHERE container = ? AND segment @> ?::segment")
      .apply(container.id, segment).singleOpt
      .map(make(segment)(container, _))
}
