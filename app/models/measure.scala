package models

import scala.concurrent.Future
import scala.collection.concurrent
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


/** Types of measurements (i.e., "units").
  * @param classification privacy-determining identification level of measurements of this type.
  * @param values possible values of categorical text data types (nominal/factors), or empty if unrestricted.
  */
sealed class Metric[T] private[models] (val id : Metric.Id, val name : String, val classification : Classification.Value, val values : IndexedSeq[String] = IndexedSeq.empty[String])(implicit val measureType : MeasureType[T]) extends TableRowId[Metric[_]] {
  // val id = id_.coerce[MetricT[T]]
  def dataType = measureType.dataType
  def sqlType : SQLType[T] = measureType.sqlType
  Metric.add(this)
}
object Metric extends TableId[Metric[_]]("metric") {
  private val cache : concurrent.Map[Int, Metric[_]] = concurrent.TrieMap.empty[Int, Metric[_]]
  protected def add(m : Metric[_]) = cache.update(m.id.unId, m)

  private[models] val row : Selector[Metric[_]] = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Classification.Value]("classification")
    , SelectColumn[DataType.Value]("type")
    , SelectColumn[Option[IndexedSeq[String]]]("values")
    ).map { (id, name, classification, dataType, values) =>
      new Metric(id, name, classification, values.getOrElse(IndexedSeq.empty[String]))(MeasureType(dataType))
    }

  /** Retrieve a single metric by id.
    * Metrics are strongly cached, so this provides a synchronous interface which may block on occasion. */
  def get(id : Id) : Option[Metric[_]] =
    cache.get(id.unId) orElse
      scala.concurrent.Await.result(
        row.SELECT("WHERE id = ?").apply(id).singleOpt,
        scala.concurrent.duration.Duration(1, scala.concurrent.duration.MINUTES))

  def getAll : Iterable[Metric[_]] =
    cache.values // XXX incomplete but assymptotically correct

  private val rowTemplate = row.from("metric JOIN record_template ON metric.id = record_template.metric")
  /** This is not used as they are for now hard-coded in RecordCategory above. */
  private def getTemplate(category : RecordCategory.Id) : Future[Seq[Metric[_]]] =
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
  final val Ident     = new Metric[String](IDENT, "ident", Classification.DEIDENTIFIED)
  /** Date of birth for any records representing organisms or other entities with dates of origination.
    * These are treated specially in combination with [[Container.date]] to compute ages.
    * [[Classification.IDENTIFIED]] implies all authorized researchers get full access to these. */
  final val Birthdate = new Metric[Date](BIRTHDATE, "birthdate", Classification.IDENTIFIED)
  /** Gender is treated as a text enumeration. */
  final val Gender    = new Metric[String](GENDER, "gender", Classification.DEIDENTIFIED, IndexedSeq[String]("Female", "Male"))
  final val Race      = new Metric[String](RACE, "race", Classification.DEIDENTIFIED, IndexedSeq[String]("American Indian or Alaska Native","Asian","Native Hawaiian or Other Pacific Islander","Black or African American","White","Multiple"))
  final val Ethnicity = new Metric[String](ETHNICITY, "ethnicity", Classification.DEIDENTIFIED, IndexedSeq[String]("Not Hispanic or Latino","Hispanic or Latino"))
  final val Language  = new Metric[String](LANGUAGE, "language", Classification.DEIDENTIFIED)
}

/** A measurement value with a specific (unconverted) type.
  * One or more measurements of distinct Metrics compose a Record. */
sealed class Measure[T](val metric : Metric[T], val datum : String) extends TableRow {
  final def metricId = metric.id
  def value : T = metric.sqlType.read(datum)
    .getOrElse(throw new SQLTypeMismatch(datum, metric.sqlType))
  def dataType = metric.dataType

  /** Add or update this measure in the database. */
  def set(record : Record) : Future[Boolean] = {
    val ids = SQLTerms('record -> record.id, 'metric -> metric.id)
    val args = ('datum -> datum) +: ids
    val tpe = metric.measureType
    DBUtil.updateOrInsert(
      SQL("UPDATE", tpe.table, "SET datum = ?::" + tpe.sqlType.name, "WHERE", ids.where)(_, _).apply(args))(
      SQL("INSERT INTO", tpe.table, "(datum, record, metric) VALUES (?::" + tpe.sqlType.name + ", ?, ?)")(_, _).apply(args))
      .execute.recover {
        case e : db.postgresql.exceptions.GenericDatabaseException if e.errorMessage.message.startsWith("invalid input syntax for type") => false
      }
  }
}
/** A measurement value with a specific (converted) type. */
final class MeasureV[T](metric : Metric[T], override val value : T) extends Measure[T](metric, metric.sqlType.show(value)) {
  override def set(record : Record) : Future[Boolean] = {
    val ids = SQLTerms('record -> record.id, 'metric -> metric.id)
    val args = SQLTerm.ofTuple('datum -> value)(metric.sqlType) +: ids
    val tpe = metric.measureType
    DBUtil.updateOrInsert(
      SQL("UPDATE", tpe.table, "SET datum = ? WHERE", ids.where)(_, _).apply(args))(
      SQL("INSERT INTO", tpe.table, args.insert)(_, _).apply(args))
      .execute
  }
}

object Measure {
  def apply[T](metric : Metric[T], value : String) =
    new Measure[T](metric, value)

  /** Retrieve the specific measure of the specified metric in the given record.
    * This does not check permissions so is unsafe.
    * @tparam T the type of the data value */
  private[models] def get[T](record : Record.Id, metric : Metric[T]) : Future[Option[MeasureV[T]]] =
    metric.measureType.select.column
      .map(new MeasureV[T](metric, _))
      .SELECT("WHERE record = ? AND metric = ?")
      .apply(record, metric.id).singleOpt

  /** Remove this measure from its associated record and delete it. */
  private[models] def remove(record : Record, metric : Metric[_]) : Future[Boolean] = {
    val args = SQLTerms('record -> record.id, 'metric -> metric.id)
    SQL("DELETE FROM", metric.measureType.table, "WHERE", args.where)
      .apply(args).execute
  }
}

/** A typed interface to measures. */
object MeasureV extends Table[MeasureV[_]]("measure_all") {
  def apply[T](metric : Metric[T], value : T) =
    new MeasureV[T](metric, value)

  private def make[T](metric : Metric[T], value : Any) =
    new MeasureV[T](metric, value.asInstanceOf[T])
  private val row : Selector[MeasureV[_]] = Selector[MeasureV[_]](
    Metric.row.selects ++ MeasureType.all.map(_.selectAll),
    "measure_all JOIN metric ON measure_all.metric = metric.id",
    new SQLLine[MeasureV[_]](Metric.row.length + MeasureType.all.length, { l =>
      val (m, dl) = l.splitAt(Metric.row.length)
      val metric = Metric.row.parse.get(m)
      val d = dl(metric.dataType.id)
      make(metric, metric.sqlType.get(d))
    }),
    Nil
  )
  
  /** Retrieve the specific measure of the specified metric in the given record.
    * This does not check permissions so is unsafe.
    * @tparam T the type of the data value */
  private[models] def get[T](record : Record.Id, metric : Metric[T]) : Future[Option[T]] =
    metric.measureType.select.column
      .SELECT("WHERE record = ? AND metric = ?")
      .apply(record, metric.id).singleOpt

  /** Retrieve the set of measures in the given record. */
  private[models] def getRecord(record : Record.Id) : Future[Seq[MeasureV[_]]] =
    row.SELECT("WHERE record = ? ORDER BY metric.id").apply(record).list
}

case class Measures(list : Seq[Measure[_]]) extends TableRow {
  private def find(id : Metric.Id) : Option[Measure[_]] =
    list.find(_.metricId.unId >= id.unId).filter(_.metricId.equals(id))

  def apply[T](metric : Metric[T]) : Option[Measure[T]] =
    find(metric.id).asInstanceOf[Option[Measure[T]]]

  def value[T](metric : Metric[T]) : Option[T] =
    apply[T](metric).map(_.value)

  def filter(c : Classification.Value) =
    Measures(list.filter(_.metric.classification >= c))
}

object Measures extends Table[Measures]("measures") {
  object empty extends Measures(Nil)
  def apply(m : Option[Measures]) : Measures =
    m.getOrElse(empty)

  private implicit val sqlType : SQLType[Measure[_]] = SQLType("measure", classOf[Measure[_]])(s =>
    Maybe(s.indexOf(':')).opt.flatMap { i =>
      Metric.get(Metric.asId(s.substring(0,i).toInt)).map(new Measure(_, s.substring(i+1)))
    },
    m => m.metricId.toString + ":" + m.datum)

  private[models] val row : Selector[Measures] =
    Columns(SelectColumn[IndexedSeq[Measure[_]]]("measures"))
    .map(new Measures(_))

  private[models] def getRecord(record : Record.Id) : Future[Measures] =
    row.SELECT("WHERE record = ?").apply(record).singleOpt.map(apply _)
}
