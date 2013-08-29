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

private[models] final class MeasureType[T] private (val dataType : DataType.Value)(implicit val column : Column[T]) {
  def name = dataType.toString
  def table = "measure_" + name
}
object MeasureType {
  implicit val measureText = new MeasureType[String](DataType.text)
  implicit val measureNumber = new MeasureType[Double](DataType.number)
  implicit val measureDate = new MeasureType[Date](DataType.date)
  def apply(dataType : DataType.Value) = dataType match {
    case DataType.text => measureText
    case DataType.number => measureNumber
    case DataType.date => measureDate
  }
}

final class MeasureData[T] private (dataType : MeasureType[T], val datum : T) {
  override def toString : String = datum.toString
}
object MeasureData {
  def apply[T](datum : T)(implicit tpe : MeasureType[T]) : MeasureData[T] =
    new MeasureData[T](tpe, datum)
}

sealed class Metric private (val id : Metric.Id, val name : String, val classification : Classification.Value, val dataType : DataType.Value, val values : Array[String] = Array[String]()) extends TableRowId[Metric] {
  def measureType = MeasureType(dataType)
}

private[models] sealed class MeasureBase(val recordId : Record.Id, val metric : Metric) extends TableRow
final class Measure[T](recordId : Record.Id, metric : Metric, val datum : MeasureData[T]) extends MeasureBase(recordId, metric)


object Metric extends TableId[Metric]("metric") {
  private[models] val row = Columns[
    Id,  String, Classification.Value, DataType.Value, Option[Array[String]]](
    'id, 'name,  'classification,      'type,          'values) map {
    (id, name, classification, tpe, values) => new Metric(id, name, classification, tpe, values.getOrElse(Array[String]()))
  }

  def get(id : Id)(implicit db : Site.DB) = id match {
    case IDENT => Some(Ident)
    case BIRTHDAY => Some(Birthday)
    case GENDER => Some(Gender)
    case _ => SELECT("WHERE id = {id}").on('id -> id).singleOpt()
  }

  private final val IDENT : Id = asId(-1)
  private final val BIRTHDAY : Id = asId(-2)
  private final val GENDER : Id = asId(-3)
  object Ident extends Metric(IDENT, "ident", Classification.DEIDENTIFIED, DataType.text)
  object Birthday extends Metric(BIRTHDAY, "birthday", Classification.IDENTIFIED, DataType.date)
  object Gender extends Metric(GENDER, "gender", Classification.DEIDENTIFIED, DataType.text, Array[String]("F", "M"))
}

object Measure extends Table[MeasureBase]("measure") {
  private val columns = Columns[
    Record.Id, Metric.Id](
    'record,   'metric)
  private[models] val row = (columns ~ Metric.row) map { 
    case ((record, _) ~ metric) => new MeasureBase(record, metric)
  }
  private def rowType[T](tpe : MeasureType[T]) = columns.~+[T]('datum)(tpe.column).inTable(tpe.table)
  
  def get[T](record : Record.Id, metric : Metric)(implicit db : Site.DB) = {
    val tpe = metric.measureType.asInstanceOf[MeasureType[T]]
    val row = rowType(tpe) map { case (record, _, datum) => new Measure[T](record, metric, MeasureData[T](datum)(tpe)) }
    SQL("SELECT " + row.select + " FROM " + tpe.table + " WHERE record = {record} AND metric = {metric}").
      on('record -> record, 'metric -> metric.id).singleOpt(row)
  }
}
