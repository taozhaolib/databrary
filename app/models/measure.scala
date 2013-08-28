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

sealed class MeasureData[T](val dataType : DataType.Value, val datum : T)
case class MeasureText(override val datum : String) extends MeasureData[String](DataType.text, datum)
case class MeasureNumber(override val datum : Double) extends MeasureData[Double](DataType.number, datum)
case class MeasureDate(override val datum : Date) extends MeasureData[Date](DataType.date, datum)
object MeasureData {
  implicit def apply(datum : String) : MeasureData[String] = MeasureText(datum)
  implicit def apply(datum : Double) : MeasureData[Double] = MeasureNumber(datum)
  implicit def apply(datum : Date) : MeasureData[Date] = MeasureDate(datum)
}

final class Metric private (val id : Metric.Id, val name : String, val classification : Classification.Value, val dataType : DataType.Value, val values : Array[String]) extends TableRowId[Metric]

case class Measure[T](val recordId : Record.Id, val metric : Metric, val datum : MeasureData[T]) {
}


object Metric extends TableId[Metric]("metric") {
  private[models] val row = Columns[
    Id,  String, Classification.Value, DataType.Value, Option[Array[String]]](
    'id, 'name,  'classification,      'type,          'values) map {
    (id, name, classification, tpe, values) => new Metric(id, name, classification, tpe, values.getOrElse(Array[String]()))
  }

  def get(id : Id)(implicit db : Site.DB) =
    SELECT("WHERE id = {id}").on('id -> id).singleOpt()
}
