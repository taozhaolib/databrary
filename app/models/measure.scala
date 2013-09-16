package models

import java.sql.Date
import anorm._
import dbrary._
import dbrary.Anorm._
import util._

/** Types of Records that are relevant for data organization.
  * Records that represent data buckets or other kinds of slot groupings (e.g., participants, days, conditions, etc.) can be assigned a particular RecordCategory for the purpose of display and templating.
  * For now there is only one instance: [RecordCategory.Participant]
  */
final class RecordCategory private (val id : RecordCategory.Id, val name : String) extends TableRowId[RecordCategory] {
  /** The default set of metrics which define records in this category. */
  def template(implicit db : Site.DB) = Metric.getTemplate(id)
}

object RecordCategory extends TableId[RecordCategory]("record_category") {
  private[models] val row = Columns[
    Id,  String](
    'id, 'name) map {
    (id, name) => id match {
      case PARTICIPANT => Participant
      case _ => new RecordCategory(id, name)
    }
  }

  def get(id : Id)(implicit db : Site.DB) : Option[RecordCategory] = id match {
    case PARTICIPANT => Some(Participant)
    case _ => SELECT("WHERE id = {id}").on('id -> id).singleOpt
  }

  def getAll(implicit db : Site.DB) : Seq[RecordCategory] =
    Seq(Participant) ++
    SELECT("WHERE id > 0 ORDER BY id").list

  private final val PARTICIPANT : Id = asId(-500)
  /** RecordCategory representing participants, individuals whose data is contained in a particular sesion.
    * Participants usually are associated with birthdate, gender, and other demographics. */
  final val Participant = new RecordCategory(PARTICIPANT, "participant")
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
  private[models] def table = "measure_" + name
  /** Column access to values of this type in the specific measurement table. */
  private[models] val column = Columns[T](SelectColumn(table, "datum"))
  /** Column access to values of this type in the joint measurement table. */
  private[models] val columnAll = Columns[T](SelectColumn("measure_all", "datum_" + name))
  private[models] def fromString(s : String) : Option[T]
}
private[models] object MeasureType {
  /** Text measurements are represented as Strings. */
  implicit val measureText = new MeasureType[String](DataType.text) {
    private[models] def fromString(s : String) = Some(s)
  }
  /** Numeric measurements are represented as Doubles, although this will lose precision.
    * BigNumeric or something might be better -- unfortunately jdbc seems to map numeric as Double anyway. */
  implicit val measureNumber = new MeasureType[Double](DataType.number) {
    private[models] def fromString(s : String) = scala.util.control.Exception.catching(classOf[java.lang.NumberFormatException]).opt(s.toDouble)
  }
  /** Date measurements. */
  implicit val measureDate = new MeasureType[Date](DataType.date) {
    private[models] def fromString(s : String) = scala.util.control.Exception.catching(classOf[java.lang.IllegalArgumentException]).opt(Date.valueOf(s))
  }

  def apply(dataType : DataType.Value) : MeasureType[_] = dataType match {
    case DataType.text => measureText
    case DataType.number => measureNumber
    case DataType.date => measureDate
  }
}


/** Abstract type for Metric. */
sealed abstract trait MetricBase extends TableRowId[MetricBase] {
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
sealed class Metric[T] private (val id : Metric.Id, val name : String, val classification : Classification.Value, val values : Array[String] = Array[String]())(implicit override val measureType : MeasureType[T]) extends MetricBase {
  val dataType = measureType.dataType
}
object Metric extends TableId[MetricBase]("metric") {
  private[this] def make(id : Metric.Id, name : String, classification : Classification.Value, dataType : DataType.Value, values : Option[Array[String]]) = id match {
    case IDENT => Ident
    case BIRTHDATE => Birthdate
    case GENDER => Gender
    case _ => new Metric(id, name, classification, values.getOrElse(Array[String]()))(MeasureType(dataType))
  }
  private[models] val row = Columns[
    Id,  String, Classification.Value, DataType.Value, Option[Array[String]]](
    'id, 'name,  'classification,      'type,          'values).map(make _)

  /** Retrieve a single metric by id. */
  def get(id : Id)(implicit db : Site.DB) : Option[MetricBase] = id match {
    case IDENT => Some(Ident)
    case BIRTHDATE => Some(Birthdate)
    case GENDER => Some(Gender)
    case _ => SELECT("WHERE id = {id}").on('id -> id).singleOpt()
  }

  def getAll(implicit db : Site.DB) : Seq[MetricBase] =
    Seq(Ident, Birthdate, Gender) ++
    SELECT("WHERE id > 0 ORDER BY id").list

  private[models] def getTemplate(category : RecordCategory.Id)(implicit db : Site.DB) : Seq[MetricBase] =
    SELECT("JOIN record_template ON id = metric WHERE category = {category} ORDER BY id").
      on('category -> category).list

  private final val IDENT : Id = asId(-900)
  private final val BIRTHDATE : Id = asId(-590)
  private final val GENDER : Id = asId(-580)
  /** Identifiers providing generic labels for records or data, such as participant id, condition name, etc.
    * [[Classification.DEIDENTIFIED]] implies these contain no identifying information, as per human subject regulations for identifiers. */
  object Ident extends Metric[String](IDENT, "ident", Classification.DEIDENTIFIED)
  /** Date of birth for any records representing organisms or other entities with dates of origination.
    * These are treated specially in combination with [[Container.date]] to compute ages.
    * [[Classification.IDENTIFIED]] implies all authorized researchers get full access to these. */
  object Birthdate extends Metric[Date](BIRTHDATE, "birthdate", Classification.IDENTIFIED)
  /** Gender is treated as a text enumeration. */
  object Gender extends Metric[String](GENDER, "gender", Classification.DEIDENTIFIED, Array[String]("F", "M"))
}


/** A dynamically-typed measurement value.
  * Should be called "MeasureDatumBase" but tha sounds silly. */
sealed trait MeasureData {
  /** The scala type of the datum. */
  type Type
  val measureType : MeasureType[Type]
  /** The value itself. */
  val value : Type
  override def toString : String = value.toString
}
/** A measurement value with a specific type. */
final class MeasureDatum[T](val value : T)(implicit val measureType : MeasureType[T]) extends MeasureData {
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
private[models] sealed abstract class MeasureBase(val recordId : Record.Id, val metric : MetricBase) extends TableRow {
  final def metricId = metric.id
  def dataType = metric.dataType
  def measureType = metric.measureType
  def datum : MeasureData

  /** Remove this measure from its associated record and delete it. */
  def remove(implicit db : Site.DB) : Unit =
    SQL("DELETE FROM " + measureType.table + " WHERE record = {record} AND metric = {metric}").
      on('record -> recordId, 'metric -> metricId).execute
}
/** A measurement with a specific, tagged type. */
final class Measure[T](recordId : Record.Id, metric : Metric[T], value_ : T) extends MeasureBase(recordId, metric) {
  private[this] var _value : T = value_
  def value = _value
  def datum = new MeasureDatum[T](value)(metric.measureType)

  /** Update the given values in the database and this object in-place. */
  def change(value : T = _value)(implicit db : Site.DB) : Unit = {
    if (value == _value)
      return
    SQL("UPDATE " + measureType.table + " SET datum = {value} WHERE record = {record} AND metric = {metric}").
      on('record -> recordId, 'metric -> metricId, 'value -> value).execute
    _value = value
  }
}

object Measure extends Table[MeasureBase]("measure_all") {
  private[this] def make[T](recordId : Record.Id, metric : Metric[T], value : Any) =
    new Measure[T](recordId, metric, value.asInstanceOf[T])
  private val columns = Columns[
    Record.Id](
    'record)
  private[models] val row : SelectParser[MeasureBase] = SelectParser[MeasureBase](
    columns.selects ++ DataType.values.map(t => SelectColumn(table, "datum_" + t.toString)) ++ Metric.row.selects,
    row => for {
      record <- columns(row)
      metric <- Metric.row(row)
      value <- metric.measureType.columnAll(row)
    } yield (make(record, metric, value))
  )
  private[models] override val src = "measure_all JOIN metric ON measure_all.metric = metric.id"
  
  /** Retrieve the specific measure of the specified metric in the given record.
    * @tparam T the type of the data value */
  private[models] def get[T](record : Record.Id, metric : Metric[T])(implicit db : Site.DB) : Option[T] = {
    val tpe = metric.measureType
    val row = tpe.column
    SQL("SELECT " + row.select + " FROM " + tpe.table + " WHERE record = {record} AND metric = {metric}").
      on('record -> record, 'metric -> metric.id).singleOpt(row)
  }

  /** Retrieve the set of measures in the given record. */
  private[models] def getRecord(record : Record.Id)(implicit db : Site.DB) : Seq[MeasureBase] =
    SELECT("WHERE record = {record} ORDER BY metric.id").on('record -> record).list

  /** Retrieve the set of all records and possibly measures of the given type on the given slot. */
  private[models] def getAnnotated[T](target : Annotated, category : Option[RecordCategory] = None, metric : Metric[T] = Metric.Ident)(implicit db : Site.DB) : Seq[(Record, Option[T])] = {
    val tpe = metric.measureType
    val row = Record.rowVolCat(target.volume, category) ~ tpe.column.? map 
      { case (r ~ m) => (r, m) }
    SQL("SELECT " + row.select + " FROM " + tpe.table +
      " RIGHT JOIN " + (if (category.isDefined) "record" else Record.src) +
      " ON record = record.id JOIN " + target.annotationTable + " ON record.id = annotation WHERE metric = {metric}" +
      (if (category.isDefined) " AND category = {category}" else "") +
      " AND " + target.annotatedLevel + " = {target}").
    on('target -> target.annotatedId, 'category -> category.map(_.id), 'metric -> metric.id).list(row)
  }

  /** Add a new measure of a specific type and metric to the given record.
    * This may fail (throw an SQLException) if there is already a value for the given metric on the record. */
  private[models] def add[T](record : Record.Id, metric : Metric[T], value : T)(implicit db : Site.DB) : Measure[T] = {
    val tpe = metric.measureType
    SQL("INSERT INTO " + tpe.table + " (record, metric, datum) VALUES ({record}, {metric}, {value})").
      on('record -> record, 'metric -> metric.id, 'value -> value).execute
    new Measure[T](record, metric, value)
  }

  private[models] def set(record : Record.Id, metric : MetricBase, value : String)(implicit db : Site.DB) : Boolean = {
    val tpe = metric.measureType
    tpe.fromString(value).fold(false) { v =>
      val args = SQLArgs('record -> record, 'metric -> metric.id, 'value -> v)
      if (SQL("UPDATE " + tpe.table + " SET datum = {value} WHERE record = {record} AND metric = {metric}").on(args : _*).executeUpdate == 0)
        SQL("INSERT INTO " + tpe.table + " (record, metric, datum) VALUES ({record}, {metric}, {value})").on(args : _*).execute
      true
    }
  }

  private[models] def delete(record : Record.Id, metric : MetricBase)(implicit db : Site.DB) : Unit = {
    val tpe = metric.measureType
    SQL("DELETE FROM " + tpe.table + " WHERE record = {record} AND metric = {metric}").
      on('record -> record, 'metric -> metric.id).execute
  }
}
