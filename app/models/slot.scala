package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsValue,JsObject,JsNull,Json}
import macros._
import macros.async._
import dbrary._
import site._

/** Conceptually a slot represents a segment of a container. */
class Slot protected (val container : Container, val segment : Segment, val consent : Consent = Consent.NONE)
  extends TableRow with InVolume with SiteObject {
  final def ===(a : Slot) : Boolean = containerId === a.containerId && segment === a.segment
  final def @>(a : Slot) : Boolean = containerId === a.containerId && segment @> a.segment
  private[models] def slotSql : SQLTerms = SQLTerms('container -> containerId, 'segment -> segment)

  final def containerId : Container.Id = container.id
  def volume = container.volume

  private[models] def withConsent(consent : Consent) =
    if (consent == this.consent) this else
    new Slot(container, segment, consent)
  /** True if this is its container's full slot. */
  def isFull : Boolean = segment.isFull
  def top : Boolean = container.top && isFull
  /** Effective start point of this slot within the container. */
  final def position : Offset = segment.lowerBound.getOrElse(Offset.ZERO)

  /** Intersect this Slot with a segment, in the current context.
    * If current context is not consented, the result may be incorrect.  */
  private[models] def *(seg : Segment) : Slot =
    if (seg @> segment) this
    else new Slot(container, segment * seg, consent)

  /** Update the given values in the database. */
  final def setConsent(consent : Consent.Value) : Future[Boolean] =
    if (consent == Consent.NONE)
      Audit.remove("slot_consent", slotSql).execute.map(_ => true)
    else
      Audit.changeOrAdd("slot_consent", SQLTerms('consent -> consent), slotSql).execute
        .recover {
          case SQLDuplicateKeyException() => false
        }

  /** The permisison level granted to restricted data within this slot. */
  final def dataPermission(classification : Classification.Value) : HasPermission =
    dataPermission(classification, consent)
  /** Whether the current user may not download restricted data within this slot. */
  final def restricted : Boolean =
    !dataPermission(Classification.RESTRICTED).checkPermission(Permission.READ)

  final def getDate : Option[org.joda.time.ReadablePartial] =
    container.date.map { date =>
      if (restricted) new org.joda.time.Partial(Permission.publicDateFields, Permission.publicDateFields.map(date.get _))
      else date
    }

  def consents : Future[Seq[ContextSlot]] = SlotConsent.getAll(this)
    SlotConsent.row.map(_(container))
    .SELECT("WHERE slot_consent.container = ? AND slot_consent.segment && ?")
    .apply(containerId, segment).list

  /** List of asset that overlap with this slot. */
  final def assets : Future[Seq[SlotAsset]] = SlotAsset.getSlot(this)

  private[models] val _records : FutureVar[Seq[(Segment, Record)]] =
    FutureVar[Seq[(Segment, Record)]](Record.getSlot(this))
  /** The list of records that apply to this slot. */
  final def records : Future[Seq[(Segment, Record)]] = _records.apply
  private[models] final def _fullRecords : FutureVar[Seq[Record]] = {
    val r = _records
    if (r.peek.isDefined)
      r.map(_.collect {
        case (seg, rec) if seg @> segment => rec
      })
    else
      FutureVar[Seq[Record]](Record.getSlotFull(this))
  }
  final def fullRecords : Future[Seq[Record]] = _fullRecords()

  /** The list of comments that apply to this slot. */
  final def comments = Comment.getSlot(this)
  /** Post a new comment this object. */
  final def postComment(text : String, parent : Option[Comment.Id] = None)(implicit site : AuthSite) : Future[Comment] =
    Comment.post(this, text, parent)

  /** The list of tags on the current slot along with the current user's applications. */
  final def tags = TagWeight.getSlot(this)
  /** Tag this slot.
    * @param vole true for up, false to remove
    * @return true if the tag name is valid
    */
  final def setTag(tag : String, vote : Boolean = true)(implicit site : AuthSite) : Future[Option[TagWeight]] =
    Tag.valid(tag).fold(async[Option[TagWeight]](None)) { tname =>
      (if (vote)
        Tag.getOrCreate(tname).flatMap { t =>
          t.add(this).map(b => if (b) Some(t) else None)
        }
      else
        Tag.get(tname).filterAsync(_.remove(this)))
      .mapAsync(_.weight(this))
    }

  def auditDownload(implicit site : Site) : Future[Boolean] =
    Audit.download("slot", 'container -> containerId, 'segment -> segment)

  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  def _idents = _fullRecords.map(_.map(_.ident))
  def idents : Future[Seq[String]] = _idents()

  private[this] def _ident : FutureVar[String] =
    _idents.map(_.mkString(" - "))

  def pageName = container.name orElse _ident.peek.flatMap(Maybe(_).opt) getOrElse {
    if (container.top)
      volume.name
    else
      "Slot"
  }
  final override def pageCrumbName : Option[String] = if (isFull) None else Some(segment.lowerBound.fold("")(_.toString) + "-" + segment.upperBound.fold("")(_.toString))
  def pageParent : Option[SitePage] = Some(container)
  def pageURL = controllers.routes.SlotHtml.view(volumeId, containerId, segment, None)

  def fileName : Future[String] =
    idents.map(i => store.fileName(container.name ++: i : _*))

  final def slotJson : JsonObject = JsonObject.flatten(
    Some('container -> container.json),
    if (segment.isFull) None else Some('segment -> segment),
    Maybe(consent).opt.map('consent -> _)
  )
  def json : JsonValue = slotJson

  private[models] def _jsonRecords(full : Boolean) : FutureVar[JsValue] =
    _records.map(JsonArray.map { case (seg, rec) =>
      JsonRecord.flatten(rec.id
      , if (seg.isFull) None else Some('segment -> seg)
      , rec.age(this).map('age -> _)
      , if (full) Some('record -> rec.json) else None)
    })
  private[models] def jsonRecords : Future[JsValue] = _jsonRecords(true)()

  final def slotJson(options : JsonOptions.Options) : Future[JsObject] =
    JsonOptions(slotJson.obj, options
    , "assets" -> (opt => assets.map(JsonArray.map(_.inContext.json - "container")))
    , "records" -> (opt => jsonRecords)
    , "tags" -> (opt => tags.map(JsonRecord.map(_.json)))
    , "comments" -> (opt => comments.map(JsonArray.map(_.json - "container")))
    , "consents" -> (opt => consents.map {
        case Seq() => JsNull
        case Seq(c) if c.segment === segment => Json.toJson(c.consent)
        case s => JsonArray(s.map { s => JsonObject(
          'segment -> s.segment,
          'consent -> s.consent)
        })
      })
    , "excerpts" -> (opt => Excerpt.getSlot(this).map(JsonArray.map(e =>
        e.json - "container" /* XXX should be on assets? - "asset" + ('asset -> e.assetId)*/)))
    )
}

class ContextSlot (container : Container, segment : Segment, consent : Consent)
  extends Slot(container, segment, consent) {
  override def consents = if (consent == Consent.NONE) super.consents else async(Seq(this))

  override def pageParent = Some(volume)
}

final class SlotConsent private (container : Container, segment : Segment, consent : Consent.Value)
  extends ContextSlot(container, segment, consent) {
  private[models] def sqlKey = slotSql
}

/** A generic type of Table that includes (presumably inheriting from) slot. */
private[models] trait TableSlot[R <: Slot] extends Table[R] {
  protected type A
  protected final def segment = SelectColumn[Segment]("segment")
  protected final def values(containerId : Container.Id, segment : Segment) =
    SQLTerms('container -> containerId, 'segment -> segment).values

  /** Generate a selector for the target type from a base selector.
    * @param columns base selector requiring a ContextSlot to produce the final type
    * @param container container selector to use
    * @param consent determine applicable consent level, or just use container context
   */
  protected final def columnsSlot(columns : Selector[(Container, Consent) => A], container : Selector[Container], consent : Boolean = true) : Selector[A] = {
    val base = columns
      .join(container, table + ".container = container.id")
    if (consent) base
      .leftJoin(SlotConsent.consent.fromAlias(table + "_consent"),
        table + ".segment <@ " + table + "_consent.segment AND " + table + ".container = " + table + "_consent.container")
      .map { case ((a, container), consent) => a(container, consent) }
    else base
      .map { case (a, container) => a(container, container.consent) }
  }
  protected final def columnsSlot(columns : Selector[(Container, Consent) => A], container : Container) : Selector[A] =
    columnsSlot(columns, Container.fixed(container), container.consent == Consent.NONE)
}

private[models] object SlotConsent extends Table[SlotConsent]("slot_consent") with TableSlot[SlotConsent] {
  private val consent = Columns(
      SelectColumn[Consent.Value]("consent")
    )
  private val columns = consent +~ segment
  private[models] val row : Selector[Container => ContextSlot] = columns
    .map { (consent, segment) =>
      if (segment.isFull) _.withConsent(consent)
      else new SlotConsent(_, segment, consent)
    }
}

private[models] abstract class SlotTable protected (table : String) extends Table[Slot](table) with TableSlot[Slot] {
  protected final type A = Slot
  private final def make(segment : Segment) : (Container, Consent) => Slot =
    if (segment.isFull) _.withConsent(_)
    else new Slot(_, segment, _)
  private[models] final val columns =
    Columns(segment)
  protected def columnsContext =
    columns.map(make)
  protected final def valueColumns(containerId : Container.Id, segment : Segment) =
    values(containerId, segment).map(_ => make(segment))
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
