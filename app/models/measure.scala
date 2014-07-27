package models

import scala.concurrent.Future
import scala.collection.concurrent
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
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
private[models] sealed abstract class MeasureType[T] private (final val dataType : DataType.Value)(implicit final val sqlType : SQLType[T]) {
  /** The name of this type, as used in database identifiers. */
  final val name = dataType.toString
  /** The table storing measurements of this type. */
  private[models] final val table = "measure_" + name
  protected implicit final val tableName : FromTable = FromTable(table)
  /** Column access to values of this type in the specific measurement table. */
  private[models] final val select : SelectColumn[T] = SelectColumn[T]("datum")
  /** Column access to values of this type in the joint measurement table. */
  private[models] final val selectAll : SelectColumn[T] = SelectColumn[T]("measure_all", "datum_" + name)
  private[models] def fromString(s : String) : T
}
private[models] object MeasureType {
  /** Text measurements are represented as Strings. */
  implicit val measureText = new MeasureType[String](DataType.text) {
    def fromString(s : String) = s
  }
  /** Numeric measurements are represented as BigDecimal. */
  implicit val measureNumber = new MeasureType[BigDecimal](DataType.number) {
    def fromString(s : String) = BigDecimal.apply(s)
  }
  /** Date measurements. */
  implicit val measureDate = new MeasureType[Date](DataType.date) {
    def fromString(s : String) = org.joda.time.LocalDate.parse(s)
  }

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
final class Metric[T] private[models] (final val id : Metric.Id, final val name : String, final val classification : Classification.Value, final val options : IndexedSeq[String] = IndexedSeq.empty[String], _assumed : Option[String] = None)(implicit final val measureType : MeasureType[T]) extends TableRowId[Metric[_]] {
  final def dataType = measureType.dataType
  final def sqlType : SQLType[T] = measureType.sqlType
  final def assumed : Option[T] = _assumed.map(measureType.fromString)
  def long : Boolean = name.equals("description")

  final def json = JsonRecord.flatten(id
    , Some('name -> name)
    , Some('type -> dataType.toString)
    , Some('classification -> classification)
    , if (options.nonEmpty) Some('options -> options) else None
    , _assumed.map('assumed -> _)
    , if (long) Some('long -> true) else None
    )
}
object Metric extends TableId[Metric[_]]("metric") {
  private val row : Selector[Metric[_]] = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Classification.Value]("classification")
    , SelectColumn[DataType.Value]("type")
    , SelectColumn[Option[IndexedSeq[String]]]("options")
    , SelectColumn[Option[String]]("assumed")
    ).map { (id, name, classification, dataType, options, assumed) =>
      implicit val mt = MeasureType(dataType)
      new Metric(id, name, classification, options.getOrElse(IndexedSeq.empty[String]), assumed)
    }

  private val list : Seq[Metric[_]] =
    async.AWAIT {
      row.SELECT("ORDER BY id").apply().list
    }
  private val byId : scala.collection.immutable.Map[Int, Metric[_]] =
    list.map(c => (c.id.unId, c)).toMap
  private val byName : scala.collection.immutable.Map[String, Metric[_]] =
    list.map(c => (c.name, c)).toMap

  /** Retrieve a single metric by id.
    * Metrics are strongly cached, so this provides a synchronous interface which may block on occasion. */
  def get(id : Id) : Option[Metric[_]] = byId.get(id.unId)
  /** Retrieve a single metric by name.
    * Like getAll, this only includes already-retrieved (by get) metrics. */
  def getName(name : String) : Option[Metric[_]] = byName.get(name)
  def _getName[T](name : String) : Metric[T] = byName(name).asInstanceOf[Metric[T]]
  /** Retrieve all metrics that have been retrieved. */
  def getAll : Iterable[Metric[_]] = list

  private[models] def getTemplate(category : RecordCategory.Id) : Future[Seq[(Metric[_], Boolean)]] =
    SQL("SELECT metric, ident FROM record_template WHERE category = ?")
    .apply(category)
    .list(SQLCols[Metric.Id, Boolean].map { (m, i) =>
      (byId(m.unId), i)
    })

  final val Ident : Metric[String] = _getName[String]("ident")
  final val Birthdate : Metric[Date] = _getName[Date]("birthdate")
}

/** A measurement value with a specific (unconverted) type.
  * One or more measurements of distinct Metrics compose a Record. */
sealed class Measure[T](val metric : Metric[T], val datum : String) {
  final def metricId = metric.id
  def ===(that : Measure[_]) : Boolean =
    metric === that.metric && datum.equals(that.datum)
  def value : T = metric.sqlType.read(datum)
    .getOrElse(throw new SQLTypeMismatch(datum, metric.sqlType))
  private[models] def dataType = metric.dataType
  private[models] def sqlArg : SQLTerm[_] = new SQLTerm("datum", datum) {
    override def placeholder = "?::" + metric.sqlType.name
  }

  /** Add or update this measure in the database. */
  private[models] def set(record : Record) : Future[Boolean] = {
    implicit val site = record.site
    Audit.changeOrAdd(metric.measureType.table, SQLTerms(sqlArg), SQLTerms('record -> record.id, 'metric -> metric.id))
      .execute.recover {
        case e : db.postgresql.exceptions.GenericDatabaseException if e.errorMessage.message.startsWith("invalid input syntax for type") => false
      }
  }
  override def toString = "Measure(" + metric.name + ", " + datum + ")"
}
/** A measurement value with a specific (converted) type. */
final class MeasureV[T](metric : Metric[T], override val value : T) extends Measure[T](metric, metric.sqlType.show(value)) {
  private[models] override def sqlArg : SQLTerm[_] = SQLTerm("datum", value)(metric.sqlType)
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
    implicit val site = record.site
    Audit.remove(metric.measureType.table, SQLTerms('record -> record.id, 'metric -> metric.id))
      .execute
  }
}

/** A typed interface to measures. */
object MeasureV extends Table[MeasureV[_]]("measure_all") {
  def apply[T](metric : Metric[T], value : T) =
    new MeasureV[T](metric, value)

  private def make[T](metric : Metric[T], value : Any) =
    new MeasureV[T](metric, value.asInstanceOf[T])
  private val row : Selector[MeasureV[_]] = {
    val mid = SelectColumn[Metric.Id]("measure_all", "metric")
    Selector[MeasureV[_]](
      mid +: MeasureType.all.map(_.selectAll),
      "measure_all",
      new SQLLine[MeasureV[_]](1 + MeasureType.all.length, { l =>
	val metric = Metric.get(mid.get(l.head)).get
	val d = l(1+metric.dataType.id)
	make(metric, metric.sqlType.get(d))
      })
    )
  }
  
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

case class Measures(list : IndexedSeq[Measure[_]]) {
  private def find(id : Metric.Id) : Option[Measure[_]] =
    list.find(_.metricId.unId >= id.unId).filter(_.metricId === id)

  def datum(metric : Metric[_]) : Option[String] =
    find(metric.id).map(_.datum)

  def apply[T](metric : Metric[T]) : Option[Measure[T]] =
    find(metric.id).asInstanceOf[Option[Measure[T]]]

  def value[T](metric : Metric[T]) : Option[T] =
    apply[T](metric).map(_.value)

  def filter(c : Classification.Value) =
    Measures(list.filter(_.metric.classification >= c))
}

object Measures extends Table[Measures]("measures") {
  val empty = Measures(IndexedSeq.empty[Measure[_]])
  def apply(m : Option[Measures]) : Measures =
    m.getOrElse(empty)

  private implicit val sqlMeasure : SQLType[Measure[_]] = SQLType("measure", classOf[Measure[_]])(s =>
    Maybe(s.indexOf(':')).opt.flatMap { i =>
      Metric.get(Metric.asId(s.substring(0,i).toInt)).map(new Measure(_, s.substring(i+1)))
    },
    m => m.metricId.toString + ":" + m.datum)

  implicit val sqlType : SQLType[Measures] =
    SQLType.transform[IndexedSeq[Measure[_]], Measures]("measures", classOf[Measures])(
      l => Some(new Measures(l)), _.list)

  private[models] val row : Selector[Measures] = Columns(
      SelectColumn[Measures]("measures")
    )

  private[models] def getRecord(record : Record.Id) : Future[Measures] =
    row.SELECT("WHERE record = ?").apply(record).singleOpt.map(apply _)

  implicit val jsonWrites : json.OWrites[Measures] =
    json.OWrites[Measures](m => json.JsObject(m.list.map(m => (m.metricId.toString, json.JsString(m.datum)))))
}
