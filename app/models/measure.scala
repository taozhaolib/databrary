package models

import java.sql.Date
import anorm._
import dbrary._
import dbrary.Anorm._
import util._

/** Types of Records that are relevant for data organization.
  * Records that represent data buckets or other kinds of slot groupings (e.g., participants, days, conditions, etc.) can be assigned a particular RecordCategory for the purpose of display.
  * For now there is only one instance: [RecordCategory.Participant]
  */
final class RecordCategory private (val id : RecordCategory.Id, val name : String) extends TableRowId[RecordCategory]

object RecordCategory extends TableId[RecordCategory]("record_category") {
  private[models] val row = Columns[
    Id,  String](
    'id, 'name) map {
    (id, name) => id match {
      case PARTICIPANT => Participant
      case _ => new RecordCategory(id, name)
    }
  }

  private final val PARTICIPANT : Id = asId(-1)
  /** RecordCategory representing participants, individuals whose data is contained in a particular sesion.
    * Participants usually are associated with birthday, gender, and other demographics. */
  final val Participant = new RecordCategory(PARTICIPANT, "participant")
}

/** Types of measurement data values. */
object DataType extends PGEnum("data_type") {
  val text, number, date = Value
}


/** Class for measurement types.
  * This provides convenient mapping tools between DataType, measures in the database, and Scala values. */
private[models] final class MeasureType[T] private (val dataType : DataType.Value)(implicit column_ : Column[T]) {
  /** The name of this type, as used in database identifiers. */
  val name = dataType.toString
  /** The table storing measurements of this type. */
  def table = "measure_" + name
  /** Column access to values of this type in the specific measurement table. */
  val column = Columns[T](SelectColumn(table, "datum"))(column_)
  /** Column access to values of this type in the joint measurement table. */
  val columnAll = Columns[T](SelectColumn("measure_all", "datum_" + name))(column_)
}
object MeasureType {
  /** Text measurements are represented as Strings. */
  implicit val measureText = new MeasureType[String](DataType.text)
  /** Numeric measurements are represented as Doubles, although this will lose precision.
    * BigNumeric or something might be better -- unfortunately jdbc seems to map numeric as Double anyway. */
  implicit val measureNumber = new MeasureType[Double](DataType.number)
  /** Date measurements. */
  implicit val measureDate = new MeasureType[Date](DataType.date)

  def apply(dataType : DataType.Value) : MeasureType[_] = dataType match {
    case DataType.text => measureText
    case DataType.number => measureNumber
    case DataType.date => measureDate
  }
}


/** Types of measurements (i.e., "units").
  * @param classification the privacy-determining identification level of measurements of this type
  * @param dataType the type of measurement values
  * @param values possible values of categorical text data types (nominal/factors), or empty if unrestricted
  */
sealed class Metric private (val id : Metric.Id, val name : String, val classification : Classification.Value, val dataType : DataType.Value, val values : Array[String] = Array[String]()) extends TableRowId[Metric] {
  def measureType = MeasureType(dataType)
}

object Metric extends TableId[Metric]("metric") {
  private[models] val row = Columns[
    Id,  String, Classification.Value, DataType.Value, Option[Array[String]]](
    'id, 'name,  'classification,      'type,          'values) map {
    (id, name, classification, tpe, values) => new Metric(id, name, classification, tpe, values.getOrElse(Array[String]()))
  }

  /** Retrieve a single metric by id. */
  def get(id : Id)(implicit db : Site.DB) = id match {
    case IDENT => Some(Ident)
    case BIRTHDAY => Some(Birthday)
    case GENDER => Some(Gender)
    case _ => SELECT("WHERE id = {id}").on('id -> id).singleOpt()
  }

  private final val IDENT : Id = asId(-1)
  private final val BIRTHDAY : Id = asId(-2)
  private final val GENDER : Id = asId(-3)
  /** Identifiers providing generic labels for records or data, such as participant id, condition name, etc.
    * [[Classification.DEIDENTIFIED]] implies these contain no identifying information, as per human subject regulations for identifiers. */
  object Ident extends Metric(IDENT, "ident", Classification.DEIDENTIFIED, DataType.text)
  /** Date of birth for any records representing organisms or other entities with dates of origination.
    * These are treated specially in combination with [[Slot.date]] to compute ages.
    * [[Classification.IDENTIFIED]] implies all authorized researchers get full access to these. */
  object Birthday extends Metric(BIRTHDAY, "birthday", Classification.IDENTIFIED, DataType.date)
  /** Gender is treated as a text enumeration. */
  object Gender extends Metric(GENDER, "gender", Classification.DEIDENTIFIED, DataType.text, Array[String]("F", "M"))
}


/** A dynamically-typed measurement value. */
sealed trait MeasureData {
  /** The scala type of the datum. */
  type Type
  val dataType : MeasureType[Type]
  /** The value itself. */
  val datum : Type
  override def toString : String = datum.toString
}
/** A measurement value with a specific type. */
final class MeasureDatum[T](val datum : T)(implicit val dataType : MeasureType[T]) extends MeasureData {
  type Type = T
}
object MeasureData {
  /* A column parser for dynamic measurement values. */
  implicit val column : Column[MeasureData] = Column.nonNull[MeasureData] { (value, meta) =>
    value match {
      case s : String => Right(new MeasureDatum(s))
      case d : Double => Right(new MeasureDatum(d))
      case d : Date => Right(new MeasureDatum(d))
      case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to MeasureData for column " + meta.column))
    }
  }
}

/** A measurement with a dynamic type.
  * One or more measurements of distinct Metrics compose a Record. */
private[models] sealed class MeasureBase(val recordId : Record.Id, val metric : Metric, val datum : MeasureData) extends TableRow
/** A measurement with a specific, tagged type. */
final class Measure[T](recordId : Record.Id, metric : Metric, datum : MeasureDatum[T]) extends MeasureBase(recordId, metric, datum)

object Measure extends Table[MeasureBase]("measure_all") {
  import MayErr._
  private val columns = Columns[
    Record.Id](
    'record)
  private[models] val row : SelectParser[MeasureBase] = SelectParser[MeasureBase](
    columns.selects ++ DataType.values.map(t => SelectColumn(table, t.toString)) ++ Metric.row.selects,
    row => for {
      record <- columns(row)
      metric <- Metric.row(row)
      tpe = metric.measureType
      datum <- tpe.columnAll(row)
    } yield (new Measure(record, metric, new MeasureDatum(datum)(tpe)))
  )
  private[models] override val src = "measure_all JOIN metric ON measure_all.metric = metric.id"
  
  /** Retrieve the specific measure of the specified metric in the given record.
    * @tparam T the type of the data value, which must match metric's type or an exception will be thrown
    */
  private[models] def get[T](record : Record.Id, metric : Metric)(implicit db : Site.DB) : Option[T] = {
    val tpe = metric.measureType.asInstanceOf[MeasureType[T]]
    val row = tpe.column
    SQL("SELECT " + row.select + " FROM " + tpe.table + " WHERE record = {record} AND metric = {metric}").
      on('record -> record, 'metric -> metric.id).singleOpt(row)
  }

  /** Retrieve the set of measures in the given record. */
  private[models] def getRecord(record : Record.Id)(implicit db : Site.DB) : Seq[MeasureBase] =
    SELECT("WHERE record = {record}").on('record -> record).list
}
