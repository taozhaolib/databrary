package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsValue,JsObject,JsNull,Json}
import macros._
import macros.async._
import dbrary._
import dbrary.SQL._
import site._

/** Conceptually a slot represents a segment of a container. */
trait Slot extends InVolume with SiteObject {
  def context : ContextSlot
  def container : Container = context.container
  def segment : Segment
  private[models] def slotSql : SQLTerms = SQLTerms('container -> containerId, 'segment -> segment)

  final def ===(a : Slot) : Boolean = containerId === a.containerId && segment === a.segment
  final def @>(a : Slot) : Boolean = containerId === a.containerId && segment @> a.segment

  final def containerId : Container.Id = container.id
  def volume = container.volume

  def consent : Consent.Value = context.consent

  /** True if this is its container's full slot. */
  def isFull : Boolean = segment.isFull
  def top : Boolean = container.top && isFull
  /** Effective start point of this slot within the container. */
  final def position : Offset = segment.lowerBound.getOrElse(Offset.ZERO)

  def consents : Future[Seq[ContextSlot]] =
    if (consent != Consent.NONE) async(Seq(context)) else
    SlotConsent.rowContainer(container)
    .SELECT(sql"WHERE slot_consent.container = $containerId AND slot_consent.segment && $segment")
    .list

  /** Update the given values in the database. */
  final def setConsent(c : Consent.Value) : Future[Boolean] =
    if (c == Consent.NONE)
      Audit.remove("slot_consent", slotSql).execute.map(_ => true)
    else
      Audit.changeOrAdd("slot_consent", SQLTerms('consent -> c), slotSql).execute
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
  final def setTag(tag : String, vote : Boolean = true, keyword : Boolean = false)(implicit site : AuthSite) : Future[Option[TagWeight]] =
    Tag.validate(tag).fold(async[Option[TagWeight]](None)) { tname =>
      (if (vote)
        Tag.getOrCreate(tname).flatMap { t =>
          t.add(this, keyword).map(b => if (b) Some(t) else None)
        }
      else
        Tag.get(tname).filterAsync(_.remove(this, keyword)))
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

  protected def consentJson : Option[JsonField] =
    if (consent == Consent.NONE) None else Some('consent -> consent)

  final def slotJson : JsonObject = JsonObject.flatten(
    Some('container -> container.json),
    if (segment.isFull) None else Some('segment -> segment),
    consentJson
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
    , "assets" -> (opt => assets.map(JsonArray.map(_.json - "container")))
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

trait ContextSlot extends Slot {
  override def context = this

  final private[models] def contextFor(seg : Segment) : ContextSlot =
    Range.nesting[Offset].tryCompare(segment, seg).map { o =>
      if (o >= 0) this else container
    }.orNull
}

final class SlotConsent private (override val container : Container, val segment : Segment, override val consent : Consent.Value)
  extends ContextSlot {
  private[models] def sqlKey = slotSql
}

/** A generic type of Table that includes (presumably inheriting from) slot. */
private[models] trait TableSlot[R <: Slot] extends Table[R] {
  protected final def segment = SelectColumn[Segment]("segment")
}

private[models] abstract class SlotTable protected (table : String) extends Table[Slot](table) with TableSlot[Slot] {
  protected final class Row (val context : ContextSlot, val segment : Segment) extends Slot {
    private[models] def sqlKey = slotSql
  }
  private[models] final val columns =
    Columns(segment)
}

object Slot extends SlotTable("slot") {
  private[models] def fixed(slot : Slot) = slot.slotSql.fixed.map(_ => slot)

  def get(container : Container, segment : Segment) : Future[Slot] =
    if (segment.isFull) async(container)
    else if (container.consent != Consent.NONE) async(new Row(container, segment))
    else ContextSlot.rowContainer(container, SQL.Arg(container.id), SQL.Arg(segment))
      .map { context =>
        new Row(context, segment)
      }
      .SELECT(sql"WHERE container.id = ${container.id}")
      .single
  def get(containerId : Container.Id, segment : Segment)(implicit site : Site) : Future[Option[Slot]] =
    if (segment.isFull) Container.get(containerId)
    else ContextSlot.rowContainer(Container.columnsVolume(Volume.row), SQL.Arg(segment))
      .map { context  =>
        new Row(context, segment)
      }
      .SELECT(sql"WHERE container.id = $containerId AND " + Volume.condition)
      .singleOpt
}

private[models] object ContextSlot {
  private[models] def rowContainer(container : Selector[Consent.Value => Container], segment : Statement) : Selector[ContextSlot] =
    container.join(SlotConsent.rowUsing(segment = segment))
    .map { case (container, consent) =>
      consent(container)
    }
  private[models] def rowContainer(container : Container, containerSql : Statement, segment : Statement) : Selector[ContextSlot] = {
    val c = Container.fixed(container).on(containerSql + " = container.id")
    if (container.consent != Consent.NONE) c
    else c.join(SlotConsent.rowUsing(segment = segment)).map { case (_, consent) =>
      consent(container)
    }
  }
}

private[models] object SlotConsent extends Table[SlotConsent]("slot_consent") with TableSlot[SlotConsent] {
  private[models] final case class Row(consent : Consent.Value, segment : Segment) {
    def apply(container : Consent.Value => Container) : ContextSlot =
      if (consent == Consent.NONE || segment.isFull) container(consent)
      else new SlotConsent(container(Consent.NONE), segment, consent)
    def apply(container : Container) : ContextSlot =
      apply(c => container.ensuring(_.consent == c))
  }
  val No = Row(Consent.NONE, Segment.empty)

  private[models] val consent = Columns(
      SelectColumn[Consent.Value]("consent")
    )
  private val columns = 
    (consent ~+ segment).map(Row.apply _)
  private[models] def rowContainer(container : Container) : Selector[ContextSlot] =
    columns.map(_(container))

  private[models] def rowUsing(container : Statement = "container.id", segment : Statement) : Selector[Row] =
    columns.on_?(container + " = slot_consent.container AND " ++ segment + " <@ slot_consent.segment")
    .map(_.getOrElse(No))
}

