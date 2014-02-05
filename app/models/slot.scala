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

  final def ===(a : Slot) : Boolean = containerId === a.containerId && segment === a.segment
  private[models] def slotSql : SQLTerms = SQLTerms('container -> containerId, 'segment -> segment)

  final def containerId : Container.Id = container.id
  def volume = container.volume

  /** True if this is its container's full slot. */
  def isFull : Boolean = segment.isFull
  def top : Boolean = container.top && isFull
  /** Effective start point of this slot within the container. */
  final def position : Offset = segment.lowerBound.getOrElse(Offset.ZERO)

  /** Intersect this Slot with a segment, in the current context.
    * If current context is not consented, the result may be incorrect.  */
  private[models] def *(seg : Segment) : Slot = Slot.make(segment * seg, context)

  /** Update the given values in the database and this object in-place. */
  final def setConsent(consent : Consent.Value) : Future[Boolean] = {
    if (consent == Consent.NONE)
      Audit.remove("slot_consent", sqlKey).execute
    else
      Audit.changeOrAdd("slot_consent", SQLTerms('consent -> consent), sqlKey).execute
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

  private[models] val _records : FutureVar[Seq[Record]] = FutureVar[Seq[Record]](Record.getSlot(this))
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
  final def setTag(tag : String, up : Option[Boolean] = Some(true))(implicit site : AuthSite) : Future[Option[TagWeight]] =
    Tag.valid(tag).fold(Async[Option[TagWeight]](None))(tname => for {
      t <- Tag.getOrCreate(tname)
      _ <- t.set(this, up)
      r <- t.weight(this)
    } yield (Some(r)))

  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  private[this] def idents : Seq[String] =
    _records.peek.fold[Seq[String]](Nil) {
      groupBy[Record,Option[RecordCategory]](_, ri => ri.category)
      .map { case (c,l) =>
        c.fold("")(_.name.capitalize + " ") + l.map(_.ident).mkString(", ")
      }
    }

  protected def ident : Option[String] =
    Maybe(idents).opt.map(_.mkString(", "))

  def pageName = container.name orElse ident getOrElse {
    if (container.top)
      volume.name
    else
      "Slot"
  }
  override def pageCrumbName : Option[String] = if (isFull) None else Some(segment.lowerBound.fold("")(_.toString) + "-" + segment.upperBound.fold("")(_.toString))
  def pageParent : Option[SitePage] = Some(container)
  def pageURL = controllers.routes.SlotHtml.view(containerId, segment)

  lazy val slotJson : JsonObject = JsonObject.flatten(
    Some('container -> container.json),
    if (segment.isFull) None else Some('segment -> segment),
    Maybe(consent).opt.map('consent -> _),
    ident.map('name -> _)
  )
  def json : JsonValue = slotJson

  def slotJson(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(slotJson.obj, options,
      "assets" -> (opt => assets.map(JsonArray.map(_.inContext.json - "container"))),
      "records" -> (opt => records.map(JsonRecord.map { r =>
        r.json ++ JsonObject.flatten(r.age(this).map('age -> _))
      })),
      "tags" -> (opt => tags.map(JsonRecord.map(_.json))),
      "comments" -> (opt => comments.map(JsonRecord.map(_.json - "container")))
    )
}

trait ContextSlot extends Slot {
  // override val container : Container
  override final def context = this
  // override val consent : Consent.Value

  override def pageParent = Some(volume)
}

final class SlotConsent private (override val container : Container, val segment : Segment, override val consent : Consent.Value) extends ContextSlot {
  private[models] def sqlKey = slotSql
}

private[models] object SlotConsent extends Table[SlotConsent]("slot_consent") {
  private val columns = Columns(
      SelectColumn[Segment]("segment")
    , SelectColumn[Consent.Value]("consent")
    )
  private[models] val row : Selector[Container => ContextSlot] = columns
    .map { (segment, consent) =>
      if (segment.isFull) { (container : Container) =>
	container._consent = consent
	container
      } else { (container : Container) =>
	new SlotConsent(container, segment, consent)
      }
    }
}

/** A generic type of Table that includes (presumably inheriting from) slot. */
private[models] trait TableSlot[R <: Slot] extends Table[R] {
  protected type A
  protected final def segment = SelectColumn[Segment]("segment")
  protected final def makeContext(container : Container, consent : Option[Container => ContextSlot]) : ContextSlot =
    consent.fold[ContextSlot](container)(_(container))
  protected final def values(containerId : Container.Id, segment : Segment) =
    SQLTerms('container -> containerId, 'segment -> segment).values

  /** Generate a selector for the target type from a base selector.
    * @param columns base selector requiring a ContextSlot to produce the final type
    * @param container container selector to use
    * @param consent determine applicable consent level, or just use container context
    */
  protected final def columnsSlot(columns : Selector[ContextSlot => A], container : Selector[Container], consent : Boolean = true) : Selector[A] = {
    val base = columns
      .join(container, table + ".container = container.id")
    if (consent) base
      .leftJoin(SlotConsent.row.fromAlias(table + "_consent"),
	table + ".segment <@ " + table + "_consent.segment AND " + table + ".container = " + table + "_consent.container")
      .map { case ((a, container), consent) => a(makeContext(container, consent)) }
    else base
      .map(tupleApply)
  }
  protected final def columnsSlot(columns : Selector[ContextSlot => A], container : Container) : Selector[A] =
    columnsSlot(columns, Container.fixed(container), container.consent == Consent.NONE)
}

private[models] abstract class SlotTable protected (table : String) extends Table[Slot](table) with TableSlot[Slot] {
  protected final type A = Slot
  private final class Virtual (val segment : Segment, val context : ContextSlot) extends Slot {
    private[models] def sqlKey = slotSql
  }
  private[models] final def make(segment : Segment, context : ContextSlot) : Slot =
    if (segment === context.segment) context
    else new Virtual(segment, context)
  private[models] final val columns =
    Columns(segment)
  protected def columnsContext =
    columns.map((make _).curried)
  protected final def valueColumns(containerId : Container.Id, segment : Segment) =
    values(containerId, segment).map(_ => make(segment, _ : ContextSlot))
  /** Generate a Slot selector from the given container selector. */
  private[models] final def rowContainer(container : Selector[Container]) =
    columnsSlot(columnsContext, container)
  private[models] final def rowContainer(container : Container) =
    columnsSlot(columnsContext, container)
  private[models] final def rowVolume(volume : Selector[Volume]) =
    rowContainer(Container.columnsVolume(volume))
  private[models] final def row(implicit site : Site) =
    rowVolume(Volume.row)
}

object Slot extends SlotTable("slot") {
  private[models] def fixed(slot : Slot) = slot.slotSql.values.map(_ => slot)

  private[models] def rowContainer(container : Container, segment : Segment) =
    columnsSlot(
      valueColumns(container.id, segment),
      container)
  private[models] def rowVolume(volume : Selector[Volume], containerId : Container.Id, segment : Segment) =
    columnsSlot(
      valueColumns(containerId, segment),
      Container.columnsVolume(volume))
  private[models] def row(containerId : Container.Id, segment : Segment)(implicit site : Site) =
    rowVolume(Volume.row, containerId, segment)

  def get(containerId : Container.Id, segment : Segment)(implicit site : Site) : Future[Option[Slot]] =
    row(containerId, segment)
    .SELECT().apply().singleOpt

  def get(container : Container, segment : Segment) : Future[Slot] =
    rowContainer(container, segment)
    .SELECT().apply().single
}
