package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
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

  def getAll : Seq[RecordCategory] =
    Seq(Participant, Visit)

  private final val PARTICIPANT : Id = asId(-500)
  private final val VISIT : Id = asId(-200)
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
  def change(category : Option[RecordCategory] = _category) : Future[Boolean] = {
    if (category == _category)
      return Async(true)
    SQL("UPDATE record SET category = ? WHERE id = ?").apply(category.map(_.id), id)
      .execute.andThen { case scala.util.Success(true) =>
        _category = category
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

  private val _daterange = FutureVar[Range[Date]] {
    SQL("SELECT record_daterange(?)").apply(id).single(SQLCols[Range[Date]].map(_.normalize))
  }
  /** The range of acquisition dates covered by associated slots. Cached. */
  def daterange : Future[Range[Date]] = _daterange.apply

  /** The range of ages as defined by `daterange - birthdate`. */
  def agerange : Future[Option[Range[Age]]] =
    Async.map[Date, Range[Age]](measures_.value(Metric.Birthdate), dob => daterange.map(_.map(d => Age(dob, d))))

  /** The age at test for a specific date, as defined by `date - birthdate`. */
  def age(date : Date) : Option[Age] =
    measures_.value(Metric.Birthdate).map(dob => Age(dob, date))

  /** The set of slots to which this record applies. */
  lazy val slots : Future[Seq[Slot]] =
    Slot.volumeRow(volume)
      .SELECT("JOIN slot_record ON slot.id = slot_record.slot WHERE slot_record.record = ? ORDER BY slot.source, slot.segment")
      .apply(id).list
  /** Attach this record to a slot. */
  def addSlot(s : Slot) = Record.addSlot(id, s.id)

  def pageName = category.fold("")(_.name.capitalize + " ") + ident
  def pageParent = Some(volume)
  def pageURL = controllers.routes.Record.view(volume.id, id)
  def pageActions = Seq(
    Action("view", controllers.routes.Record.view(volumeId, id), Permission.VIEW),
    Action("edit", controllers.routes.Record.edit(volumeId, id), Permission.EDIT)
  )
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
  private[models] def volumeRow(vol : Volume) =
    columns map {
      rec => rec(vol)
    }
  private[models] def measureRow[T](vol : Volume, metric : Metric[T]) = {
    val mt = metric.measureType
    volumeRow(vol).leftJoin(mt.select.column, "record.id = " + mt.table + ".record AND " + mt.table + ".metric = ?")
  }
  private def sessionRow(vol : Volume) = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Option[RecordCategory.Id]]("category")
    ).leftJoin(Measures.row, "record.id = measures.record")
    .?.join(Slot.volumeRow(vol).?, _ + " JOIN slot_record ON record.id = slot_record.record FULL JOIN " + _ + " ON slot_record.slot = slot.id AND container.volume = record.volume")
    .map {
      case (Some(((id, cat), meas)), slot) =>
        val r = new Record(id, vol, cat.flatMap(RecordCategory.get(_)), slot.fold(Consent.NONE)(_.consent), Measures(meas))
        r._daterange.set(slot.flatMap(_.container.date).fold[Range[Date]](Range.empty)(Range.singleton _))
        (slot, Some(r))
      case (None, slot) =>
        (slot, None)
    }

  /** Retrieve a specific record by id. */
  def get(id : Id)(implicit site : Site) : Future[Option[Record]] =
    row.SELECT("WHERE record.id = ? AND", Volume.condition)
      .apply(id +: Volume.conditionArgs).singleOpt

  /** Retrieve the set of records on the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[Record]] =
    volumeRow(slot.volume)
      .SELECT("JOIN slot_record ON record.id = slot_record.record WHERE slot_record.slot = ? ORDER BY record.category")
      .apply(slot.id).list

  /** Retrieve all the categorized records associated with the given volume.
    * @param category restrict to the specified category, or include all categories
    * @return records sorted by category, ident */
  private[models] def getVolume(volume : Volume, category : Option[RecordCategory] = None) : Future[Seq[Record]] =
    volumeRow(volume)
      .SELECT("WHERE record.volume = ?",
        (if (category.isDefined) "AND record.category = ?" else ""),
        "ORDER BY", (if (category.isEmpty) "record.category, " else ""))
      .apply(volume.id +: category.fold(SQLArgs())(c => SQLArgs(c.id))).list

  /** Return the full outer product of all slot, record pairs on the given volume for "session" slots and categorized records. */
  private[models] def getSessions(vol : Volume) : Future[Seq[(Option[Slot],Option[Record])]] =
    sessionRow(vol)
      .SELECT("WHERE container.volume = ? AND (slot.consent IS NOT NULL OR slot.segment = '(,)') AND NOT container.top OR record.volume = ? AND record.category IS NOT NULL")
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
    SQL("INSERT INTO record " + args.insert + " RETURNING id")
      .apply(args).single(SQLCols[Id].map(new Record(_, volume, category)))
  }

  private[models] def addSlot(r : Record.Id, s : Slot.Id) : Future[Boolean] = {
    val args = SQLTerms('record -> r, 'slot -> s)
    SQL("INSERT INTO slot_record " + args.insert)
      .apply(args).execute.recover {
        case SQLDuplicateKeyException() => false
      }
  }
  private[models] def removeSlot(r : Record.Id, s : Slot.Id) : Unit =
    DELETE('record -> r, 'slot -> s).run()
}
