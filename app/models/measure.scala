package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.github.mauricio.async.db
import macros._
import dbrary._
import site._

/** Types of measurement data values. */
object DataType extends PGEnum("data_type") {
  val text, number, date = Value
}

/** Class for measurement types.
  * This provides convenient mapping tools between DataType, measures in the database, and Scala values. */
private[models] final class MeasureType[T] private (val dataType : DataType.Value)(implicit val sqlType : SQLType[T]) {
  /** The name of this type, as used in database identifiers. */
  val name = dataType.toString
  /** The table storing measurements of this type. */
  private[models] val table = "measure_" + name
  protected implicit val tableName : FromTable = FromTable(table)
  /** Column access to values of this type in the specific measurement table. */
  private[models] val select : SelectColumn[T] = SelectColumn[T]("datum")
  /** Column access to values of this type in the joint measurement table. */
  private[models] val selectAll : SelectColumn[T] = SelectColumn[T]("measure_all", "datum_" + name)
}
private[models] object MeasureType {
  /** Text measurements are represented as Strings. */
  implicit val measureText = new MeasureType[String](DataType.text)
  /** Numeric measurements are represented as BigDecimal. */
  implicit val measureNumber = new MeasureType[BigDecimal](DataType.number)
  /** Date measurements. */
  implicit val measureDate = new MeasureType[Date](DataType.date)

  val all : IndexedSeq[MeasureType[_]] = IndexedSeq(
    measureText,
    measureNumber,
    measureDate)
    
  def apply(dataType : DataType.Value) : MeasureType[_] = all(dataType.id)
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
    case LANGUAGE => Language
    case _ => new MetricT(id, name, classification, values.getOrElse(Array[String]()))(MeasureType(dataType))
  }
  private[models] val row : Selector[MetricT[_]] = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Classification.Value]("classification")
    , SelectColumn[DataType.Value]("type")
    , SelectColumn[Option[Array[String]]]("values")
    ).map(make _)

  /** Retrieve a single metric by id. */
  def get(id : Id) : Future[Option[Metric]] = id match {
    case IDENT => Async(Some(Ident))
    case BIRTHDATE => Async(Some(Birthdate))
    case GENDER => Async(Some(Gender))
    case RACE => Async(Some(Race))
    case ETHNICITY => Async(Some(Ethnicity))
    case LANGUAGE => Async(Some(Language))
    case _ => row.SELECT("WHERE id = ?").apply(id).singleOpt
  }

  def getAll : Future[Seq[Metric]] =
    row.SELECT("WHERE id > 0 ORDER BY id").apply().list map
      (Seq(Ident, Birthdate, Gender, Race, Ethnicity, Language) ++ _)

  private val rowTemplate = row.from("metric JOIN record_template ON metric.id = record_template.metric")
  /** This is not used as they are for now hard-coded in RecordCategory above. */
  private def getTemplate(category : RecordCategory.Id) : Future[Seq[Metric]] =
    rowTemplate.SELECT("WHERE record_template.category = ? ORDER BY metric.id")
      .apply(category).list

  private final val IDENT     : Id = asId(-900)
  private final val BIRTHDATE : Id = asId(-590)
  private final val GENDER    : Id = asId(-580)
  private final val RACE      : Id = asId(-550)
  private final val ETHNICITY : Id = asId(-540)
  private final val LANGUAGE  : Id = asId(-510)
  /** Identifiers providing generic labels for records or data, such as participant id, condition name, etc.
    * [[Classification.DEIDENTIFIED]] implies these contain no identifying information, as per human subject regulations for identifiers. */
  object Ident     extends MetricT[String](IDENT, "ident", Classification.DEIDENTIFIED)
  /** Date of birth for any records representing organisms or other entities with dates of origination.
    * These are treated specially in combination with [[Container.date]] to compute ages.
    * [[Classification.IDENTIFIED]] implies all authorized researchers get full access to these. */
  object Birthdate extends MetricT[Date](BIRTHDATE, "birthdate", Classification.IDENTIFIED)
  /** Gender is treated as a text enumeration. */
  object Gender    extends MetricT[String](GENDER, "gender", Classification.DEIDENTIFIED, Array[String]("Female", "Male"))
  object Race      extends MetricT[String](RACE, "race", Classification.DEIDENTIFIED, Array[String]("American Indian or Alaska Native","Asian","Native Hawaiian or Other Pacific Islander","Black or African American","White","Multiple"))
  object Ethnicity extends MetricT[String](ETHNICITY, "ethnicity", Classification.DEIDENTIFIED, Array[String]("Not Hispanic or Latino","Hispanic or Latino"))
  object Language extends MetricT[String](LANGUAGE, "language", Classification.DEIDENTIFIED)
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
  implicit object sqlType extends SQLType[MeasureDatumT[_]]("datum", classOf[MeasureDatumT[_]]) {
    override def put(d : MeasureDatumT[_]) = d.value
    override def get(x : Any, where : String = "") : MeasureDatumT[_] = x match {
      case null => throw new SQLUnexpectedNull(this, where)
      case s : String => new MeasureDatumT[String](s)
      case d : BigDecimal => new MeasureDatumT[BigDecimal](d)
      case d : Date => new MeasureDatumT[Date](d)
      case _ => throw new SQLTypeMismatch(x, this, where)
    }
    override def read(s : String) = Some(new MeasureDatumT[String](s))
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
  protected def ids : SQLTerms = SQLTerms('record -> recordId, 'metric -> metricId)

  /** Add or update this measure in the database. */
  def set : Future[Boolean]
  /** Remove this measure from its associated record and delete it. */
  def remove : Future[Boolean] =
    Measure.delete(recordId, metric)
}
/** A measurement with a specific, tagged type. */
final case class MeasureT[T : SQLType](override val recordId : Record.Id, override val metric : MetricT[T], value : T) extends MeasureBase(recordId, metric) {
  def datum = new MeasureDatumT[T](value)(metric.measureType)
  override def stringValue = value.toString

  def set : Future[Boolean] = {
    val ids = this.ids
    val args = ('datum -> value) +: ids
    val tpe = measureType
    DBUtil.updateOrInsert(
      SQL("UPDATE " + tpe.table + " SET datum = ? WHERE " + ids.where)(_, _).apply(args))(
      SQL("INSERT INTO " + tpe.table + " " + args.insert)(_, _).apply(args)).execute
  }
}
/** A measurement with an arbitrary (unconverted) type. */
final case class Measure(override val recordId : Record.Id, override val metric : Metric, value : String) extends MeasureBase(recordId, metric) {
  def datum = new MeasureDatum(value)
  override def stringValue = value

  def set : Future[Boolean] = {
    val ids = this.ids
    val args = ('value -> value) +: ids
    val tpe = metric.measureType
    DBUtil.updateOrInsert(
      SQL("UPDATE " + tpe.table + " SET datum = ?::" + tpe.sqlType.name + " WHERE " + ids.where)(_, _).apply(args))(
      SQL("INSERT INTO " + tpe.table + " (datum, record, metric) VALUES (?::" + tpe.sqlType.name + ", ?, ?)")(_, _).apply(args))
      .execute.recover {
        case e : db.postgresql.exceptions.GenericDatabaseException if e.errorMessage.message.startsWith("invalid input syntax for type") => false
      }
  }
}

private[models] sealed abstract class MeasureView[R <: MeasureBase](table : String) extends Table[R](table) {
  protected val columns = Columns(
    SelectColumn[Record.Id]("record")
  )
  protected val row : Selector[R]
  
  /** Retrieve the set of measures in the given record. */
  private[models] def getRecord(record : Record.Id) : Future[Seq[R]] =
    row.SELECT("WHERE record = ? ORDER BY metric.id").apply(record).list

  private[models] def delete(record : Record.Id, metric : Metric) : Future[Boolean] = {
    val tpe = metric.measureType
    val args = SQLTerms('record -> record, 'metric -> metric.id)
    SQL("DELETE FROM " + metric.measureType.table + " WHERE " + args.where)
      .apply(args).execute
  }
}

/** A typed interface to measures. */
object MeasureT extends MeasureView[MeasureT[_]]("measure_all") {
  private[this] def make[T](recordId : Record.Id, metric : MetricT[T], value : Any) =
    new MeasureT[T](recordId, metric, value.asInstanceOf[T])(metric.measureType.sqlType)
  protected val row : Selector[MeasureT[_]] = Selector[MeasureT[_]](
    columns.selects ++ Metric.row.selects ++ MeasureType.all.map(_.selectAll),
    "measure_all JOIN metric ON measure_all.metric = metric.id",
    new SQLLine[MeasureT[_]](columns.length + Metric.row.length + MeasureType.all.length, { l =>
      val (c, md) = l.splitAt(columns.length)
      val (m, dl) = md.splitAt(Metric.row.length)
      val record = columns.parse.get(c)
      val metric = Metric.row.parse.get(m)
      val d = dl(metric.dataType.id)
      val sqlt = metric.measureType.sqlType
      make(record, metric, sqlt.get(d))
    }),
    Nil
  )
  
  /** Retrieve the specific measure of the specified metric in the given record.
    * This does not check permissions so is unsafe.
    * @tparam T the type of the data value */
  private[models] def get[T](record : Record.Id, metric : MetricT[T]) : Future[Option[T]] =
    metric.measureType.select.column
      .SELECT("WHERE record = ? AND metric = ?")
      .apply(record, metric.id).singleOpt

  /** Retrieve the set of all categorized records and possibly measures of the given type on the given slot. */
  private[models] def getSlot[T](slot : Slot, category : Option[RecordCategory] = None, metric : MetricT[T] = Metric.Ident) : Future[Seq[(Record, Option[T])]] =
    Record.measureRow[T](slot.volume, metric)
      .SELECT("JOIN slot_record ON record.id = slot_record.record",
        "AND record.category", (if (category.isDefined) "= ?" else " IS NOT NULL"),
        "AND slot_record.slot = ? ORDER BY record.category").
      apply(metric.id +: category.fold(SQLArgs())(c => SQLArgs(c.id)) :+ slot.id).list
}

/** A un-typed interface to measures. */
object Measure extends MeasureView[Measure]("measure_view") {
  private[this] def make(metric : Metric)(recordId : Record.Id, value : String) =
    new Measure(recordId, metric, value)
  private val dataColumns = columns ~+ SelectColumn[String]("datum")
  protected val row = dataColumns.join(Metric.row, "measure_view.metric = metric.id") map {
    case (meas, metric) => (make(metric) _).tupled(meas)
  }
  private[models] def metricRow(metric : Metric) = dataColumns.map(make(metric) _)
  
  /** Retrieve the specific measure of the specified metric in the given record.
    * This does not check permissions so is unsafe. */
  private[models] def get(record : Record.Id, metric : Metric) : Future[Option[Measure]] =
    metricRow(metric).SELECT("WHERE record = ? AND metric = ?")
      .apply(record, metric.id).singleOpt
}
