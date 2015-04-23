package models

import scala.concurrent.Future
import scala.collection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import com.github.mauricio.async.db
import macros._
import dbrary._
import dbrary.SQL._
import site._

/** Types of measurement data values. */
object DataType extends SQL.Enum("data_type") {
  val text, number, date = Value
}

/** Class for measurement types.
  * This provides convenient mapping tools between DataType, measures in the database, and Scala values. */
private[models] sealed abstract class MeasureType[T] private (final val dataType : DataType.Value)(implicit final val sqlType : SQL.Type[T]) {
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
  implicit object measureText extends MeasureType[String](DataType.text) {
    def fromString(s : String) = s
  }
  /** Numeric measurements are represented as BigDecimal. */
  implicit object measureNumber extends MeasureType[BigDecimal](DataType.number) {
    def fromString(s : String) = BigDecimal.apply(s)
  }
  /** Date measurements. */
  implicit object measureDate extends MeasureType[Date](DataType.date) {
    def fromString(s : String) = org.joda.time.LocalDate.parse(s)
  }

  val all : IndexedSeq[MeasureType[_]] = IndexedSeq(
    measureText,
    measureNumber,
    measureDate)
    
  def apply(dataType : DataType.Value) : MeasureType[_] = all(dataType.id)
}


/** Types of measurements (i.e., "units").
  * @param release privacy-determining identification level of measurements of this type.
  * @param values possible values of categorical text data types (nominal/factors), or empty if unrestricted.
  */
final class Metric[T] private[models] (final val id : Metric.Id, final val name : String, final val release : Release.Value, final val options : IndexedSeq[String] = IndexedSeq.empty[String], _assumed : Option[String] = None)(implicit final val measureType : MeasureType[T])
  extends TableRowId[Metric[_]] {
  final def dataType = measureType.dataType
  final def sqlType : SQL.Type[T] = measureType.sqlType
  final def assumed : Option[T] = _assumed.map(measureType.fromString)
  def long : Boolean = name.equals("description")

  final def json = JsonRecord.flatten(id
    , Some('name -> name)
    , Some('type -> dataType.toString)
    , Maybe(release).opt('release -> _)
    , if (options.nonEmpty) Some('options -> options) else None
    , _assumed.map('assumed -> _)
    , if (long) Some('long -> true) else None
    )
}
object Metric extends TableId[Metric[_]]("metric") {
  private val row : Selector[Metric[_]] = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Release.Value]("release")
    , SelectColumn[DataType.Value]("type")
    , SelectColumn[Option[IndexedSeq[String]]]("options")
    , SelectColumn[Option[String]]("assumed")
    ).map { (id, name, release, dataType, options, assumed) =>
      implicit val mt = MeasureType(dataType)
      new Metric(id, name, release, options.getOrElse(IndexedSeq.empty[String]), assumed)
    }

  private val list : Seq[Metric[_]] =
    async.AWAIT {
      row.SELECT(lsql"ORDER BY id").list
    }
  private val byId : TableIdMap[Metric[_]] =
    TableIdMap(list : _*)
  private val byName : collection.immutable.Map[String, Metric[_]] =
    list.map(c => (c.name, c)).toMap

  /** Retrieve a single metric by id.
    * Metrics are strongly cached, so this provides a synchronous interface which may block on occasion. */
  def get(id : Id) : Option[Metric[_]] = byId.get(id)
  /** Retrieve a single metric by name.
    * Like getAll, this only includes already-retrieved (by get) metrics. */
  def getName(name : String) : Option[Metric[_]] = byName.get(name)
  def _getName[T](name : String) : Metric[T] = byName(name).asInstanceOf[Metric[T]]
  /** Retrieve all metrics that have been retrieved. */
  def getAll : Iterable[Metric[_]] = list

  private[models] def getTemplate(category : RecordCategory.Id) : Future[Seq[(Metric[_], Boolean)]] =
    lsql"SELECT metric, ident FROM record_template WHERE category = $category"
    .run.list(SQL.Cols[Metric.Id, Boolean].map { (m, i) =>
      (byId(m), i)
    })

  final val ID : Metric[String] = _getName[String]("ID")
  final val Birthdate : Metric[Date] = _getName[Date]("birthdate")
}

/** A measurement value with a specific (unconverted) type.
  * One or more measurements of distinct Metrics compose a Record. */
sealed class Measure[T](val metric : Metric[T], val datum : String) {
  final def metricId = metric.id
  def ===(that : Measure[_]) : Boolean =
    metric === that.metric && datum.equals(that.datum)
  def value : T = metric.sqlType.read(datum)
    .getOrElse(throw new SQL.TypeMismatch(datum, metric.sqlType))
  private[models] def dataType = metric.dataType
  private[models] def sqlArg : SQLTerm[_] = new SQLTerm("datum", datum) {
    override def statement = "?::" + metric.sqlType.name
  }

  /** Add or update this measure in the database. */
  private[models] def set(record : Record) : Future[Boolean] = {
    implicit val site = record.site
    Audit.changeOrAdd(metric.measureType.table, SQLTerms(sqlArg), SQLTerms('record -> record.id, 'metric -> metric.id))
      .execute.recover {
        case e : db.postgresql.exceptions.GenericDatabaseException if e.errorMessage.message.startsWith("invalid input syntax for type") || e.errorMessage.message.startsWith("date/time field value out of range") => false
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
      .SELECT(sql"WHERE record = $record AND metric = ${metric.id}")
      .singleOpt

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
      ",measure_all",
      new SQL.Line[MeasureV[_]](1 + MeasureType.all.length, { l =>
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
    .SELECT(sql"WHERE record = $record AND metric = ${metric.id}")
    .singleOpt

  /** Retrieve the set of measures in the given record. */
  private[models] def getRecord(record : Record.Id) : Future[Seq[MeasureV[_]]] =
    row.SELECT(sql"WHERE record = $record ORDER BY metric.id").list
}

class MeasuresView protected (private val map : collection.Map[Metric[_],Measure[_]]) {
  def list : Iterable[Measure[_]] =
    map.values

  private def find(m : Metric[_]) : Option[Measure[_]] =
    map.get(m)

  def datum(metric : Metric[_]) : Option[String] =
    find(metric).map(_.datum)

  def apply[T](metric : Metric[T]) : Option[Measure[T]] =
    find(metric).asInstanceOf[Option[Measure[T]]]

  def value[T](metric : Metric[T]) : Option[T] =
    apply[T](metric).map(_.value)

  def filter(f : Release.Value => Boolean) =
    new MeasuresView(map.filterKeys(m => f(m.release)))
}

final class Measures private (private val map : collection.mutable.Map[Metric[_],Measure[_]]) extends MeasuresView(map) {
  private[models] def update(m : Measure[_]) {
    map.update(m.metric, m)
  }

  private[models] def remove(m : Metric[_]) {
    map -= m
  }
}

object MeasuresView {
  implicit val jsonWrites : json.OWrites[MeasuresView] =
    json.OWrites[MeasuresView](m => json.JsObject(m.map.toSeq.map {
      case (m, v) => (m.id.toString, json.JsString(v.datum))
    }))
}

object Measures extends Table[Measures]("measures") {
  def empty = new Measures(collection.mutable.Map.empty[Metric[_],Measure[_]])
  private def apply(m : Option[Measures]) : Measures =
    m.getOrElse(empty)
  private def apply(l : Seq[Measure[_]]) : Measures =
    new Measures(collection.mutable.Map(l.map(m => m.metric -> m) : _*))

  private implicit val sqlMeasure : SQL.Type[Measure[_]] =
    SQL.Type("measure", classOf[Measure[_]])(s =>
      for {
        i <- Maybe(s.indexOf(':')).opt()
        id <- Metric.get(Metric.asId(s.substring(0,i).toInt))
      } yield (new Measure(id, s.substring(i+1))),
      m => m.metricId.toString + ":" + m.datum)

  implicit val sqlType : SQL.Type[Measures] =
    SQL.Type.transform[IndexedSeq[Measure[_]], Measures]("measures", classOf[Measures])(
      l => Some(apply(l)), _.map.values.toIndexedSeq)

  private[models] val row : Selector[Measures] = Columns(
      SelectColumn[Measures]("measures")
    )

  private[models] def getRecord(record : Record.Id) : Future[Measures] =
    row.SELECT(sql"WHERE record = $record").singleOpt.map(apply _)
}
