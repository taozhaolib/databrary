package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import macros._
import dbrary._
import site._

/** Types of Records that are relevant for data organization.
  * Records that represent data buckets or other kinds of slot groupings (e.g., participants, days, conditions, etc.) can be assigned a particular RecordCategory for the purpose of display and templating.
  * For now, all instances are hard-coded.
  */
sealed abstract class RecordCategory private (val id : RecordCategory.Id, val name : String) extends TableRowId[RecordCategory] {
  /** The default set of metrics which define records in this category. */
  def template : Seq[Metric[_]]
}

/** Interface to record categories.
  * These are all hard-coded so bypass the database, though they are stored in record_category. */
object RecordCategory extends HasId[RecordCategory] {
  def get(id : Id) : Option[RecordCategory] = id match {
    case PARTICIPANT => Some(Participant)
    case VISIT => Some(Visit)
    case _ => None
  }
  
  def getName(name : String) : Option[RecordCategory] = name match {
    case "participant" => Some(Participant)
    case "visit" => Some(Visit)
    case _ => None
  }

  def getAll : Seq[RecordCategory] =
    Seq(Participant, Visit)

  def getVolume(volume : Volume) : Future[Seq[RecordCategory]] =
    SQL("SELECT DISTINCT category FROM record WHERE volume = ? AND category IS NOT NULL")
      .apply(volume.id)
      .list(SQLCols[RecordCategory.Id].map(get(_).get))

  final val PARTICIPANT : Id = asId(-500)
  final val VISIT : Id = asId(-200)
  /** RecordCategory representing participants, individuals whose data is contained in a particular sesion.
    * Participants usually are associated with birthdate, gender, and other demographics. */
  final val Participant = new RecordCategory(PARTICIPANT, "participant") {
    val template = Seq(Metric.Ident, Metric.Birthdate, Metric.Gender, Metric.Race, Metric.Ethnicity)
  }
  final val Visit = new RecordCategory(VISIT, "visit") {
    val template = Seq(Metric.Ident)
  }
}

/** A set of Measures. */
final class Record private (val id : Record.Id, val volume : Volume, val category_ : Option[RecordCategory] = None, val consent : Consent.Value = Consent.NONE, measures_ : Measures = Measures.empty) extends TableRowId[Record] with SiteObject with InVolume {
  private[this] var _category = category_
  def category : Option[RecordCategory] = _category
  def categoryId = category.map(_.id)

  /** Update the given values in the database and this object in-place. */
  def change(category : Option[Option[RecordCategory]] = None) : Future[Boolean] = {
    category.fold(Async(false)) { cat =>
    SQL("UPDATE record SET category = ? WHERE id = ?").apply(cat.map(_.id), id)
      .execute.andThen { case scala.util.Success(true) =>
        _category = cat
      }
    }
  }

  /** The set of measures on the current volume readable by the current user. */
  lazy val measures : Measures =
    Classification.download(volume.permission, consent).fold[Measures](Measures.empty)(measures_.filter _)

  /** Add or change a measure on this record.
    * This is not type safe so may generate SQL exceptions, and may invalidate measures on this object. */
  def setMeasure[T](metric : Metric[T], value : String) : Future[Boolean] =
    Measure[T](metric, value).set(this)
  /** Add or change a measure on this record. */
  def setMeasureV[T](metric : Metric[T], value : T) : Future[Boolean] =
    MeasureV[T](metric, value).set(this)
  /** Remove a measure from this record.
    * This may invalidate measures on this object. */
  def removeMeasure(metric : Metric[_]) = Measure.remove(this, metric)

  def ident : String = measures.value(Metric.Ident).getOrElse("[" + id + "]")

  /** The age at test for a specific date, as defined by `date - birthdate`. */
  def age(date : Date) : Option[Age] =
    measures_.value(Metric.Birthdate).map(dob => Age(dob, date))

  /** The age at test during a specific slot, with privacy limits applied. */
  def age(slot : Slot) : Option[Age] =
    slot.container.date.flatMap(age(_).map { a =>
      if (a > Age.LIMIT && !slot.downloadable) Age.LIMIT
      else a
    })

  /** The set of slots to which this record applies. */
  lazy val slots : Future[Seq[Slot]] =
    SlotRecord.slots(this)
  /** Attach this record to a slot. */
  def addSlot(s : Slot) : Future[Boolean] =
    SlotRecord.add(this, s)
      .recover {
        case SQLDuplicateKeyException() => false
      }
  /** Remove this record from a slot. */
  def removeSlot(s : Slot) : Future[Boolean] =
    SlotRecord.remove(this, s)

  /** The set of assets to which this record applies. */
  def assets : Future[Seq[SlotAsset]] =
    SlotAsset.getRecord(this)

  def pageName = category.fold("")(_.name.capitalize + " ") + ident
  def pageParent = Some(volume)
  def pageURL = controllers.routes.RecordHtml.view(id)
  def pageActions = Seq(
    Action("view", pageURL, Permission.VIEW),
    Action("edit", controllers.routes.RecordHtml.edit(id), Permission.EDIT)
  )

  lazy val json : JsonRecord =
    JsonRecord.flatten(id,
      Some('volume -> volumeId),
      category.map('category -> _.name),
      Some('measures -> measures)
    )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options,
      "slots" -> (opt => slots.map(JsonArray.map(s =>
        s.json ++ JsonObject.flatten(age(s).map('age -> _))
      )))
    )
}

private[models] object SlotRecord extends SlotTable("slot_record") {
  def row(record : Record) =
    rowContainer(Container.volumeRow(record.volume))

  def slots(record : Record) =
    row(record)
    .SELECT("WHERE slot_record.record = ? AND container.volume = ? ORDER BY slot_record.container, slot_record.segment")
    .apply(record.id, record.volumeId).list

  def add(record : Record, slot : Slot) =
    INSERT(('record -> record.id) +: slot.sql).execute
  def remove(record : Record, slot : Slot) =
    DELETE(('record -> record.id) +: slot.sql).execute
}

object Record extends TableId[Record]("record") {
  private val columns : Selector[Volume => Record] = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Option[RecordCategory.Id]]("category")
    , SelectAs[Consent.Value]("record_consent(record.id)", "record_consent")
    ).leftJoin(Measures.row, "record.id = measures.record")
    .map { case ((id, cat, cons), meas) =>
      vol => new Record(id, vol, cat.flatMap(RecordCategory.get(_)), cons, Measures(meas))
    }
  private def row(implicit site : Site) =
    columns.join(Volume.row, "record.volume = volume.id") map {
      case (rec, vol) => rec(vol)
    }
  private def volumeRow(vol : Volume) = columns.map(_(vol))
  private def measureRow[T](vol : Volume, metric : Metric[T]) = {
    val mt = metric.measureType
    volumeRow(vol).leftJoin(mt.select.column, "record.id = " + mt.table + ".record AND " + mt.table + ".metric = ?")
  }
  private def sessionRow(vol : Volume) = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Option[RecordCategory.Id]]("category")
    ).leftJoin(Measures.row, "record.id = measures.record")
    .join(Container.volumeRow(vol), _ + " JOIN slot_record ON record.id = slot_record.record JOIN slot ON slot_record.slot = slot.id JOIN " + _ + " ON slot.source = container.id AND record.volume = container.volume")
    .map {
      case (((id, cat), meas), cont) =>
        val r = new Record(id, vol, cat.flatMap(RecordCategory.get(_)), cont.consent, Measures(meas))
        (r, cont)
    }

  /** Retrieve a specific record by id. */
  def get(id : Id)(implicit site : Site) : Future[Option[Record]] =
    row.SELECT("WHERE record.id = ? AND", Volume.condition)
      .apply(id).singleOpt

  /** Retrieve the list of all records that apply to the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[Record]] =
    volumeRow(slot.volume)
      .SELECT("JOIN slot_record ON record.id = slot_record.record JOIN slot ON slot_record.slot = slot.id WHERE slot.source = ? AND slot.segment && ?::segment AND record.volume = ?")
      .apply(slot.containerId, slot.segment, slot.volumeId).list

  /** Retrieve the list of all foreign records (from a different volume) that apply to the given slot. */
  private[models] def getSlotForeign(slot : Slot)(implicit site : Site) : Future[Seq[Record]] =
    row
      .SELECT("JOIN slot_record ON record.id = slot_record.record JOIN slot ON slot_record.slot = slot.id WHERE slot.source = ? AND slot.segment && ?::segment AND record.volume <> ? AND", Volume.condition)
      .apply(slot.containerId, slot.segment, slot.volumeId).list

  /** Retrieve all the categorized records associated with the given volume.
    * @param category restrict to the specified category, or include all categories
    * @return records sorted by category, ident */
  private[models] def getVolume(volume : Volume, category : Option[RecordCategory] = None) : Future[Seq[Record]] =
    volumeRow(volume)
      .SELECT("WHERE record.volume = ?",
        (if (category.isDefined) "AND record.category = ?" else "ORDER BY record.category"))
      .apply(volume.id +: category.fold(SQLArgs())(c => SQLArgs(c.id))).list

  /** Return the full outer product of all slot, record pairs on the given volume for "session" slots and categorized records. */
  private[models] def getSessions(vol : Volume) : Future[Seq[(Record,Container)]] =
    sessionRow(vol)
      .SELECT("WHERE record.volume = ? AND record.category IS NOT NULL OR container.volume = ? AND NOT container.top ORDER BY record.category NULLS LAST, record.id")
      .apply(vol.id, vol.id).list

  /** Retrieve the records in the given volume with a measure of the given value.
    * @param category restrict to the specified category, or include all categories
    * @param metric search by metric
    * @param value measure value that must match
    */
  def findMeasure[T](volume : Volume, category : Option[RecordCategory] = None, metric : Metric[T], value : T) : Future[Seq[Record]] =
    measureRow[T](volume, metric).map(_._1)
      .SELECT("WHERE record.volume = ?",
        (if (category.isDefined) "AND record.category = ?" else ""),
        "AND", metric.measureType.select.toString, "= ?")
      .apply(SQLArgs(metric.id, volume.id) ++ category.fold(SQLArgs())(c => SQLArgs(c.id)) ++ SQLArgs(value)(metric.measureType.sqlType)).list

  /** Create a new record, initially unattached. */
  def create(volume : Volume, category : Option[RecordCategory] = None) : Future[Record] = {
    val args = SQLTerms('volume -> volume.id, 'category -> category.map(_.id))
    SQL("INSERT INTO record", args.insert, "RETURNING id")
      .apply(args).single(SQLCols[Id].map(new Record(_, volume, category)))
  }
}
