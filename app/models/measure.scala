package models

import java.sql.Date
import anorm._
import dbrary._
import dbrary.Anorm._
import util._

/** Types of Records that are relevant for data organization.
  * Records that represent data buckets or other kinds of slot groupings (e.g., participants, days, conditions, etc.) can be assigned a particular RecordCategory for the purpose of display and templating.
  * For now, all instances are hard-coded.
  */
sealed abstract class RecordCategory private (val id : RecordCategory.Id, val name : String) extends TableRowId[RecordCategory] {
  /** The default set of metrics which define records in this category. */
  def template : Seq[Metric]
}

/** Interface to record categories.
  * These are all hard-coded so bypass the database, though they are stored in record_category. */
object RecordCategory extends HasId[RecordCategory] {
  def get(id : Id) : Option[RecordCategory] = id match {
    case PARTICIPANT => Some(Participant)
    case _ => None
  }

  def getAll : Seq[RecordCategory] =
    Seq(Participant)

  private final val PARTICIPANT : Id = asId(-500)
  /** RecordCategory representing participants, individuals whose data is contained in a particular sesion.
    * Participants usually are associated with birthdate, gender, and other demographics. */
  final val Participant = new RecordCategory(PARTICIPANT, "participant") {
    val template = Seq(Metric.Ident, Metric.Birthdate, Metric.Gender, Metric.Race, Metric.Ethnicity)
  }
}

/** Types of measurement data values. */
object DataType extends PGEnum("data_type") {
  val text, number, date = Value
}


/** Class for measurement types.
  * This provides convenient mapping tools between DataType, measures in the database, and Scala values. */
private[models] abstract sealed class MeasureType[T : Column] private (val dataType : DataType.Value) {
  /** The name of this type, as used in database identifiers. */
  val name = dataType.toString
  /** The table storing measurements of this type. */
  private[models] val table = "measure_" + name
  protected implicit val tableName : FromTable = FromTable(table)
  /** Column access to values of this type in the specific measurement table. */
  private[models] val column = Columns[T](SelectColumn("datum"))
  /** Column access to values of this type in the joint measurement table. */
  private[models] val columnAll = Columns[T](SelectColumn("measure_all", "datum_" + name))
  private[models] def fromString(s : String) : Option[T]
  private[models] val pgType : String
}
private[models] object MeasureType {
  /** Text measurements are represented as Strings. */
  implicit val measureText = new MeasureType[String](DataType.text) {
    private[models] def fromString(s : String) = Some(s)
    private[models] val pgType = "text"
  }
  /** Numeric measurements are represented as Doubles, although this will lose precision.
    * BigNumeric or something might be better -- unfortunately jdbc seems to map numeric as Double anyway. */
  implicit val measureNumber = new MeasureType[Double](DataType.number) {
    private[models] def fromString(s : String) = scala.util.control.Exception.catching(classOf[java.lang.NumberFormatException]).opt(s.toDouble)
    private[models] val pgType = "numeric"
  }
  /** Date measurements. */
  implicit val measureDate = new MeasureType[Date](DataType.date) {
    private[models] def fromString(s : String) = scala.util.control.Exception.catching(classOf[java.lang.IllegalArgumentException]).opt(Date.valueOf(s))
    private[models] val pgType = "date"
  }

  def apply(dataType : DataType.Value) : MeasureType[_] = dataType match {
    case DataType.text => measureText
    case DataType.number => measureNumber
    case DataType.date => measureDate
  }
}


/** Abstract type for Metric. */
sealed abstract trait Metric extends TableRowId[Metric] {
  val id : Metric.Id
  val name : String
  /** The privacy-determining identification level of measurements of this type. */
  val classification : Classification.Value
  val dataType : DataType.Value
  /** possible values of categorical text data types (nominal/factors), or empty if unrestricted. */
  val values : Array[String]
  def measureType = MeasureType(dataType)
}
/** Types of measurements (i.e., "units"). */
sealed class MetricT[T] private[models] (id_ : Metric.Id, val name : String, val classification : Classification.Value, val values : Array[String] = Array[String]())(implicit override val measureType : MeasureType[T]) extends Metric with TableRowId[MetricT[T]] {
  val id = id_.coerce[MetricT[T]]
  val dataType = measureType.dataType
}
object Metric extends TableId[MetricT[_]]("metric") {
  private[this] def make(id : Metric.Id, name : String, classification : Classification.Value, dataType : DataType.Value, values : Option[Array[String]]) = id match {
    case IDENT => Ident
    case BIRTHDATE => Birthdate
    case GENDER => Gender
    case RACE => Race
    case ETHNICITY => Ethnicity
    case _ => new MetricT(id, name, classification, values.getOrElse(Array[String]()))(MeasureType(dataType))
  }
  private[models] val row = Columns[
    Id,  String, Classification.Value, DataType.Value, Option[Array[String]]](
    'id, 'name,  'classification,      'type,          'values).map(make _)

  /** Retrieve a single metric by id. */
  def get(id : Id)(implicit db : Site.DB) : Option[Metric] = id match {
    case IDENT => Some(Ident)
    case BIRTHDATE => Some(Birthdate)
    case GENDER => Some(Gender)
    case RACE => Some(Race)
    case ETHNICITY => Some(Ethnicity)
    case _ => row.SQL("WHERE id = {id}").on('id -> id).singleOpt
  }

  def getAll(implicit db : Site.DB) : Seq[Metric] =
    Seq(Ident, Birthdate, Gender, Race, Ethnicity) ++
    row.SQL("WHERE id > 0 ORDER BY id").list

  private val rowTemplate = row.from("metric JOIN record_template ON metric.id = record_template.metric")
  /** This is not used as they are for now hard-coded in RecordCategory above. */
  private def getTemplate(category : RecordCategory.Id)(implicit db : Site.DB) : Seq[Metric] =
    rowTemplate.SQL("WHERE record_template.category = {category} ORDER BY metric.id").
      on('category -> category).list

  private final val IDENT     : Id = asId(-900)
  private final val BIRTHDATE : Id = asId(-590)
  private final val GENDER    : Id = asId(-580)
  private final val RACE      : Id = asId(-550)
  private final val ETHNICITY : Id = asId(-540)
  /** Identifiers providing generic labels for records or data, such as participant id, condition name, etc.
    * [[Classification.DEIDENTIFIED]] implies these contain no identifying information, as per human subject regulations for identifiers. */
  object Ident     extends MetricT[String](IDENT, "ident", Classification.DEIDENTIFIED)
  /** Date of birth for any records representing organisms or other entities with dates of origination.
    * These are treated specially in combination with [[Container.date]] to compute ages.
    * [[Classification.IDENTIFIED]] implies all authorized researchers get full access to these. */
  object Birthdate extends MetricT[Date](BIRTHDATE, "birthdate", Classification.IDENTIFIED)
  /** Gender is treated as a text enumeration. */
  object Gender    extends MetricT[String](GENDER, "gender", Classification.DEIDENTIFIED, Array[String]("F", "M"))
  object Race      extends MetricT[String](RACE, "race", Classification.DEIDENTIFIED, Array[String]("American Indian or Alaska Native","Asian","Native Hawaiian or Other Pacific Islander","Black or African American","White","Multiple"))
  object Ethnicity extends MetricT[String](ETHNICITY, "ethnicity", Classification.DEIDENTIFIED, Array[String]("Not Hispanic or Latino","Hispanic or Latino"))
}


/** A dynamically-typed measurement value. */
sealed trait MeasureDatumBase {
  /** The scala type of the datum. */
  type Type
  // val measureType : MeasureType[Type]
  /** The value itself. */
  val value : Type
  override def toString : String = value.toString
}
/** A measurement value with a specific type. */
final class MeasureDatumT[T](val value : T)(implicit val measureType : MeasureType[T]) extends MeasureDatumBase {
  type Type = T
}
/** A measurement value with an arbitrary (unconverted) type. */
final class MeasureDatum(val value : String) extends MeasureDatumBase {
  type Type = String
}
object MeasureDatum {
  /** A column parser for dynamic measurement values. */
  implicit val column : Column[MeasureDatumT[_]] = Column.nonNull[MeasureDatumT[_]] { (value, meta) =>
    value match {
      case s : String => Right(new MeasureDatumT(s))
      case d : Double => Right(new MeasureDatumT(d))
      case d : Date => Right(new MeasureDatumT(d))
      case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to MeasureDatum for column " + meta.column))
    }
  }
}

/** A measurement with a dynamic type.
  * One or more measurements of distinct Metrics compose a Record. */
private[models] sealed abstract class MeasureBase(val recordId : Record.Id, val metric : Metric) extends TableRow {
  final def metricId = metric.id
  def dataType = metric.dataType
  def measureType = metric.measureType
  def datum : MeasureDatumBase
  def stringValue : String = datum.toString
  protected def ids : SQLArgs = SQLArgs('record -> recordId, 'metric -> metricId)

  /** Add or update this measure in the database. */
  def set(implicit db : Site.DB) : Boolean
  /** Remove this measure from its associated record and delete it. */
  def remove(implicit db : Site.DB) : Unit =
    Measure.delete(recordId, metric)
}
/** A measurement with a specific, tagged type. */
final case class MeasureT[T](override val recordId : Record.Id, override val metric : MetricT[T], value : T) extends MeasureBase(recordId, metric) {
  def datum = new MeasureDatumT[T](value)(metric.measureType)
  override def stringValue = value.toString

  def set(implicit db : Site.DB) : Boolean = {
    val ids = this.ids
    val args = ids ++ SQLArgs('datum -> value)
    val tpe = measureType
    DBUtil.updateOrInsert(
      SQL("UPDATE " + tpe.table + " SET datum = {datum} WHERE " + ids.where).on(args : _*))(
      SQL("INSERT INTO " + tpe.table + " " + args.insert).on(args : _*))
    true
  }
}
/** A measurement with an arbitrary (unconverted) type. */
final case class Measure(override val recordId : Record.Id, override val metric : Metric, value : String) extends MeasureBase(recordId, metric) {
  def datum = new MeasureDatum(value)
  override def stringValue = value

  def set(implicit db : Site.DB) : Boolean = {
    val ids = this.ids
    val args = ids ++ SQLArgs('value -> value)
    val tpe = metric.measureType
    val sp = db.setSavepoint
    try {
      DBUtil.updateOrInsert(
        SQL("UPDATE " + tpe.table + " SET datum = {value}::" + tpe.pgType + " WHERE " + ids.where).on(args : _*))(
        SQL("INSERT INTO " + tpe.table + " (record, metric, datum) VALUES ({record}, {metric}, {value}::" + tpe.pgType + ")").on(args : _*))
      true
    } catch {
      case e : java.sql.SQLException if e.getMessage.startsWith("ERROR: invalid input syntax for type") =>
        db.rollback(sp)
        false
    } finally {
      db.releaseSavepoint(sp)
    }
  }
}

private[models] sealed abstract class MeasureView[R <: MeasureBase](table : String) extends Table[R](table) {
  protected val columns = Columns[
    Record.Id](
    'record)
  
  /** Retrieve the set of measures in the given record. */
  private[models] def getRecord(record : Record.Id)(implicit db : Site.DB) : Seq[R] =
    row.SQL("WHERE record = {record} ORDER BY metric.id").on('record -> record).list

  private[models] def delete(record : Record.Id, metric : Metric)(implicit db : Site.DB) : Unit = {
    val tpe = metric.measureType
    val args = SQLArgs('record -> record, 'metric -> metric.id)
    SQL("DELETE FROM " + metric.measureType.table + " WHERE " + args.where).
      on(args :_*).execute
  }
}

/** A typed interface to measures. */
object MeasureT extends MeasureView[MeasureT[_]]("measure_all") {
  private[this] def make[T](recordId : Record.Id, metric : MetricT[T], value : Any) =
    new MeasureT[T](recordId, metric, value.asInstanceOf[T])
  private[models] val row : Selector[MeasureT[_]] = Selector[MeasureT[_]](
    columns.selects ++ DataType.values.map(t => SelectColumn("datum_" + t.toString)) ++ Metric.row.selects,
    "measure_all JOIN metric ON measure_all.metric = metric.id",
    row => for {
      record <- columns(row)
      metric <- Metric.row(row)
      value <- metric.measureType.columnAll(row)
    } yield (make(record, metric, value))
  )
  
  /** Retrieve the specific measure of the specified metric in the given record.
    * This does not check permissions so is unsafe.
    * @tparam T the type of the data value */
  private[models] def get[T](record : Record.Id, metric : MetricT[T])(implicit db : Site.DB) : Option[T] = {
    val tpe = metric.measureType
    tpe.column.SQL("WHERE record = {record} AND metric = {metric}").
      on('record -> record, 'metric -> metric.id).singleOpt
  }

  /** Retrieve the set of all records and possibly measures of the given type on the given slot. */
  private[models] def getSlot[T](slot : Slot, category : Option[RecordCategory] = None, metric : MetricT[T] = Metric.Ident)(implicit db : Site.DB) : Seq[(Record, Option[T])] =
    Record.measureRow[T](slot.volume, metric).
      SQL("JOIN slot_record ON record.id = slot_record.record",
        (if (category.isDefined) " AND record.category = {category}" else ""),
        "AND slot_record.slot = {slot}").
      on('slot -> slot.id, 'category -> category.map(_.id), 'metric -> metric.id).
      list
}

/** A un-typed interface to measures. */
object Measure extends MeasureView[Measure]("measure_view") {
  private[this] def make(metric : Metric)(recordId : Record.Id, value : String) =
    new Measure(recordId, metric, value)
  private val dataColumns = columns.~+[String]('datum)
  private[models] val row = dataColumns.join(Metric.row, "measure_view.metric = metric.id") map {
    case (meas ~ metric) => (make(metric) _).tupled(meas)
  }
  private[models] def metricRow(metric : Metric) = dataColumns.map(make(metric) _)
  
  /** Retrieve the specific measure of the specified metric in the given record.
    * This does not check permissions so is unsafe. */
  private[models] def get(record : Record.Id, metric : Metric)(implicit db : Site.DB) : Option[Measure] =
    metricRow(metric).SQL("WHERE record = {record} AND metric = {metric}").
      on('record -> record, 'metric -> metric.id).singleOpt
}
