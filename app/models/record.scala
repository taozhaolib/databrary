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
sealed class RecordCategory private (val id : RecordCategory.Id, val name : String) extends TableRowId[RecordCategory] {
  /** The default set of metrics which define records in this category. */
  val ident : Seq[Metric[_]] = Seq(Metric.Ident)
  def template : Seq[Metric[_]] = ident

  val json = JsonRecord(id,
      'name -> name
    )
}

/** Interface to record categories.
  * These are all hard-coded so bypass the database, though they are stored in record_category. */
object RecordCategory extends TableId[RecordCategory]("record_category") {
  def get(id : Id) : Option[RecordCategory] =
    byId.get(id.unId)
  
  def getName(name : String) : Option[RecordCategory] =
    byName.get(name)

  def getAll : Seq[RecordCategory] =
    list

  def getVolume(volume : Volume) : Future[Seq[RecordCategory]] =
    SQL("SELECT DISTINCT category FROM record WHERE volume = ? AND category IS NOT NULL")
      .apply(volume.id)
      .list(SQLCols[RecordCategory.Id].map(get(_).get))

  final val PILOT       : Id = asId(-800)
  final val EXCLUSION   : Id = asId(-700)
  final val PARTICIPANT : Id = asId(-500)
  final val CONDITION   : Id = asId(-400)
  final val TASK        : Id = asId(-300)
  final val GROUP       : Id = asId(-200)
  final val LOCATION    : Id = asId(-100)

  final val Pilot = new RecordCategory(PILOT, "pilot")
  final val Exclusion = new RecordCategory(EXCLUSION, "exclusion") {
    override val ident = Seq(Metric.Reason)
  }
  /** RecordCategory representing participants, individuals whose data is contained in a particular sesion.
    * Participants usually are associated with birthdate, gender, and other demographics. */
  final val Participant = new RecordCategory(PARTICIPANT, "participant") {
    override val template = Seq(Metric.Ident, Metric.Birthdate, Metric.Gender, Metric.Race, Metric.Ethnicity)
  }
  final val Condition = new RecordCategory(CONDITION, "condition")
  final val Task = new RecordCategory(TASK, "task") {
    override val template = Seq(Metric.Ident, Metric.Description)
  }
  final val Group = new RecordCategory(GROUP, "group")
  final val Location = new RecordCategory(LOCATION, "location") {
    override val ident = Seq(Metric.Setting, Metric.State, Metric.Country)
    override val template = Seq(Metric.Setting, Metric.State)
  }

  private val list = Seq(Pilot, Exclusion, Participant, Condition, Task, Group, Location)
  private val byId = Map[Int, RecordCategory](list.map(c => (c.id.unId, c)) : _*)
  private val byName = Map[String, RecordCategory](list.map(c => (c.name, c)) : _*)
}

/** A set of Measures. */
final class Record private (val id : Record.Id, val volume : Volume, val category_ : Option[RecordCategory] = None, val consent : Consent.Value = Consent.NONE, measures_ : Measures = Measures.empty) extends TableRowId[Record] with SiteObject with InVolume {
  private[this] var _category = category_
  def category : Option[RecordCategory] = _category
  def categoryId = category.map(_.id)

  /** Update the given values in the database and this object in-place. */
  def change(category : Option[Option[RecordCategory]] = None) : Future[Boolean] = {
    category.fold(async(false)) { cat =>
    SQL("UPDATE record SET category = ? WHERE id = ?").apply(cat.map(_.id), id)
      .execute.andThen { case scala.util.Success(true) =>
        _category = cat
      }
    }
  }

  /** The set of measures on the current volume readable by the current user. */
  lazy val measures : Measures =
    Classification.read(volume.permission, consent).fold[Measures](Measures.empty)(measures_.filter _)

  /** Add or change a measure on this record.
    * This is not type safe so may generate SQL exceptions, and may invalidate measures on this object. */
  def setMeasure[T](measure : Measure[T]) : Future[Boolean] =
    measure.set(this)
  /** Remove a measure from this record.
    * This may invalidate measures on this object. */
  def removeMeasure(metric : Metric[_]) = Measure.remove(this, metric)

  def ident : String =
    Maybe(category.fold[Seq[Metric[_]]](Seq(Metric.Ident))(_.ident)
      .flatMap(measures.datum(_)))
    .fold(category.fold("[" + id + "]")(_.name))(_.mkString(", "))

  /** The age at test for a specific date, as defined by `date - birthdate`. */
  def age(date : Date) : Option[Age] =
    measures_.value(Metric.Birthdate).map(dob => Age(dob, date))

  /** The age at test during a specific slot, with privacy limits applied. */
  def age(slot : Slot) : Option[Age] =
    slot.container.date.flatMap(age(_).map { a =>
      if (a > Age.LIMIT && slot.restricted) Age.LIMIT
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

  def pageName = category.fold("")(_.name.capitalize + " ") + ident
  def pageParent = Some(volume)
  def pageURL = controllers.routes.RecordHtml.view(id)

  lazy val json : JsonRecord =
    JsonRecord.flatten(id,
      Some('volume -> volumeId),
      category.map('category -> _.id),
      Some('measures -> measures)
    )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options,
      "slots" -> (opt => slots.map(JsonArray.map(s =>
        s.slotJson ++ JsonObject.flatten(age(s).map('age -> _))
      )))
    )
}

private[models] object SlotRecord extends SlotTable("slot_record") {
  def row(record : Record) =
    rowContainer(Container.columnsVolume(Volume.fixed(record.volume)))

  def slots(record : Record) =
    row(record)
    .SELECT("WHERE slot_record.record = ? AND container.volume = ? ORDER BY container.top DESC, slot_record.container, slot_record.segment")
    .apply(record.id, record.volumeId).list

  def add(record : Record, slot : Slot) =
    INSERT(('record -> record.id) +: slot.slotSql).execute
  def remove(record : Record, slot : Slot) =
    DELETE(('record -> record.id) +: slot.slotSql).execute
}

object Record extends TableId[Record]("record") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Option[RecordCategory.Id]]("category")
    , SelectColumn[Measures]("measures")
    ).from("record_measures AS " + _)
  private[models] def sessionRow(vol : Volume) = columns
    .map { case (id, cat, meas) =>
      (consent : Consent.Value) =>
	new Record(id, vol, cat.flatMap(RecordCategory.get(_)), consent, meas)
    }
  private def rowVolume(volume : Selector[Volume]) : Selector[Record] = columns
    .~(SelectAs[Consent.Value]("record_consent(record.id)", "record_consent"))
    .join(volume, "record.volume = volume.id")
    .map { case (((id, cat, meas), cons), vol) =>
      new Record(id, vol, cat.flatMap(RecordCategory.get(_)), cons, meas)
    }
  private def rowVolume(vol : Volume) : Selector[Record] =
    rowVolume(Volume.fixed(vol))
  private def row(implicit site : Site) =
    rowVolume(Volume.row)

  /** Retrieve a specific record by id. */
  def get(id : Id)(implicit site : Site) : Future[Option[Record]] =
    row.SELECT("WHERE record.id = ? AND", Volume.condition)
      .apply(id).singleOpt

  /** Retrieve the list of all records that apply to the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[Record]] =
    rowVolume(slot.volume)
    .SELECT("JOIN slot_record ON record.id = slot_record.record WHERE slot_record.container = ? AND slot_record.segment && ?::segment ORDER BY record.category NULLS LAST, record.id")
    .apply(slot.containerId, slot.segment).list

  /** Retrieve all the categorized records associated with the given volume.
    * @param category restrict to the specified category, or include all categories
    * @return records sorted by category, ident */
  private[models] def getVolume(volume : Volume, category : Option[RecordCategory] = None) : Future[Seq[Record]] =
    rowVolume(volume)
    .SELECT(if (category.isDefined) "WHERE record.category = ?" else "ORDER BY record.category")
    .apply(category.fold(SQLArgs())(c => SQLArgs(c.id))).list

  /** Retrieve the records in the given volume with a measure of the given value.
    * @param category restrict to the specified category, or include all categories
    * @param metric search by metric
    * @param value measure value that must match
    */
  def findMeasures[T](volume : Volume, category : Option[RecordCategory], measures : Measure[T]*) : Future[Seq[Record]] = {
    measures.zipWithIndex.foldLeft(rowVolume(volume)) { (s, mi) =>
      val (m, i) = mi
      val ma = "m_" + i.toString
      val mt = m.metric.measureType
      s.join(mt.select.column.fromAlias(ma),
	"record.id = " + ma + ".record AND " + ma + ".metric = ? AND " + ma + ".datum = ?")
      .pushArgs(SQLArgs(m.metric.id) :+ m.sqlArg)
      .map(_._1)
    }
    .SELECT(if (category.isDefined) "WHERE record.category = ?" else "")
    .apply(category.fold(SQLArgs())(c => SQLArgs(c.id))).list
  }

  /** Create a new record, initially unattached. */
  def create(volume : Volume, category : Option[RecordCategory] = None) : Future[Record] = {
    val args = SQLTerms('volume -> volume.id, 'category -> category.map(_.id))
    SQL("INSERT INTO record", args.insert, "RETURNING id")
      .apply(args).single(SQLCols[Id].map(new Record(_, volume, category)))
  }
}
