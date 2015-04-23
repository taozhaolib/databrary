package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import macros._
import dbrary._
import dbrary.SQL._
import site._

/** Types of Records that are relevant for data organization.
  * Records that represent data buckets or other kinds of slot groupings (e.g., participants, days, conditions, etc.) can be assigned a particular RecordCategory for the purpose of display and templating.
  * For now, all instances are hard-coded.
  */
final class RecordCategory private (val id : RecordCategory.Id, val name : String) extends TableRowId[RecordCategory] {
  /** The default set of metrics which define records in this category. */
  private lazy val templates =
    async.AWAIT(Metric.getTemplate(id))
  final def ident : Seq[Metric[_]] = templates.filter(_._2).map(_._1)
  final def template : Seq[Metric[_]] = templates.map(_._1)

  final val json = JsonRecord(id
    , 'name -> name
    , 'ident -> ident.map(_.id)
    , 'template -> template.map(_.id)
    )
}

/** Interface to record categories.
  * These are all hard-coded so bypass the database, though they are stored in record_category. */
object RecordCategory extends TableId[RecordCategory]("record_category") {
  private[this] val row = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    ) map { (id, name) =>
      new RecordCategory(id, name)
    }

  private[this] val list : Seq[RecordCategory] =
    async.AWAIT {
      row.SELECT(lsql"ORDER BY id").list
    }
  private[this] val byId : TableIdMap[RecordCategory] =
    TableIdMap(list : _*)
  private[this] val byName : collection.immutable.Map[String, RecordCategory] =
    list.map(c => (c.name, c)).toMap

  def get(id : Id) : Option[RecordCategory] = byId.get(id)
  def getName(name : String) : Option[RecordCategory] = byName.get(name)
  def getAll : Seq[RecordCategory] = list

  def getVolume(volume : Volume) : Future[Seq[RecordCategory]] =
    sql"SELECT DISTINCT category FROM record WHERE volume = ${volume.id} AND category IS NOT NULL ORDER BY category"
    .run.list(SQL.Cols[RecordCategory.Id].map(byId(_)))

  val Participant = byName("participant")
}

/** A set of Measures. */
final class Record private (val id : Record.Id, val volume : Volume, val category : Option[RecordCategory] = None, val release : Release.Value = Release.DEFAULT, measures_ : Measures = Measures.empty) extends TableRowId[Record] with SiteObject with InVolume {
  def categoryId = category.map(_.id)

  /** Update the given values in the database and this object in-place. */
  def change(category : Option[Option[RecordCategory]] = None) : Future[Record] =
    Audit.change("record", SQLTerms.flatten(
        category.map('category -> _.map(_.id))),
      sqlKey)
      .ensure.map { _ =>
        new Record(id, volume, category.getOrElse(this.category), release, measures_)
      }

  /** The set of measures on the current volume readable by the current user. */
  lazy val measures : MeasuresView =
    Release.read(permission).fold[MeasuresView](Measures.empty)(r => measures_.filter(Maybe(_).orElse(release) >= r))

  /** Add or change a measure on this record.
    * This is not type safe so may generate SQL exceptions. */
  def setMeasure[T](measure : Measure[T]) : Future[Boolean] =
    measure.set(this).andThen { case scala.util.Success(true) =>
      measures_.update(measure)
    }
  /** Remove a measure from this record. */
  def removeMeasure(metric : Metric[_]) =
    Measure.remove(this, metric).andThen { case scala.util.Success(true) =>
      measures_.remove(metric)
    }

  def ident : String =
    Maybe(category.fold[Seq[Metric[_]]](Seq(Metric.ID))(_.ident)
      .flatMap(measures.datum(_)))
    .fold(category.fold("[" + id + "]")(_.name))(_.mkString(", "))

  /** The age at test for a specific date, as defined by `date - birthdate`. */
  def age(date : Date) : Option[Age] =
    measures_.value(Metric.Birthdate).map(dob => Age(dob, date))

  /** The age at test during a specific slot, with privacy limits applied. */
  def age(slot : Slot) : Option[Age] =
    slot.container.date.flatMap(age(_).map { a =>
      if (a > Age.LIMIT && slot.restricted) Age.LIMIT
      else a
    })

  /** The set of slots to which this record applies. */
  lazy val slots : Future[Seq[Slot]] =
    SlotRecord.slots(this)

  def pageName = category.fold("")(_.name.capitalize + " ") + ident
  def pageParent = Some(volume)
  def pageURL = controllers.routes.RecordHtml.view(id)

  lazy val json : JsonRecord =
    JsonRecord.flatten(id,
      Some('volume -> volumeId),
      category.map('category -> _.id),
      Some('measures -> measures)
    )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options,
      "slots" -> (opt => slots.map(JsonArray.map(s =>
        s.slotJson ++ JsonObject.flatten(age(s).map('age -> _))
      )))
    )
}

object SlotRecord extends SlotTable("slot_record") {
  private[models] def slots(record : Record) =
    columns
    .join(ContextSlot.rowContainer(
      Container.columnsVolume(Volume.fixed(record.volume)) on "slot_record.container = container.id",
      "slot_record.segment"))
    .map { case (segment, context) =>
      new Row(context, segment)
    }
    .SELECT(sql"WHERE slot_record.record = ${record.id} ORDER BY container.top DESC, slot_record.container, slot_record.segment")
    .list

  def move(record : Record, container : Container, src : Segment = Segment.empty, dst : Segment = Segment.empty) : Future[Boolean] = {
    implicit val site = record.site
    val key = SQLTerms('record -> record.id, 'container -> container.id)
    (if (src.isEmpty) {
      if (dst.isEmpty) return async(false)
      Audit.add(table, key :+ ('segment -> dst))
    } else if (dst.isEmpty) {
      Audit.remove(table, key :+ SQLTerm.eq("segment", "&&", src))
    } else
      Audit.change(table, SQLTerms('segment -> dst), key :+ ('segment -> src)))
    .execute.recover {
      case SQLDuplicateKeyException() => false
    }
  }
}

object Record extends TableId[Record]("record") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Option[RecordCategory.Id]]("category")
    , SelectColumn[Measures]("measures")
    ).mapFrom("record_measures AS " +: _)
  private[models] def sessionRow(vol : Volume) = columns
    .map { case (id, cat, meas) =>
      (release : Release.Value) =>
        new Record(id, vol, cat.flatMap(RecordCategory.get(_)), release, meas)
    }
  private def rowVolume(volume : Selector[Volume]) : Selector[Record] = columns
    .~(SelectAs[Release.Value]("record_release(record.id)", "record_release"))
    .join(volume on "record.volume = volume.id")
    .map { case (((id, cat, meas), cons), vol) =>
      new Record(id, vol, cat.flatMap(RecordCategory.get(_)), cons, meas)
    }
  private[models] def rowVolume(vol : Volume) : Selector[Record] =
    rowVolume(Volume.fixed(vol))
  private def row(implicit site : Site) =
    rowVolume(Volume.row)

  /** Retrieve a specific record by id. */
  def get(id : Id)(implicit site : Site) : Future[Option[Record]] =
    row.SELECT(sql"WHERE record.id = $id AND " + Volume.condition)
    .singleOpt

  /** Retrieve the list of all records that cover the given slot. */
  private[models] def getSlotFull(slot : Slot) : Future[Seq[Record]] =
    rowVolume(slot.volume)
    .SELECT(sql"JOIN slot_record ON record.id = slot_record.record WHERE slot_record.container = ${slot.containerId} AND slot_record.segment @> ${slot.segment}::segment ORDER BY record.category NULLS LAST")
    .list

  /** Retrieve the list of all records that apply to the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[(Segment,Record)]] =
    SlotRecord.columns
    .join(rowVolume(slot.volume) on "slot_record.record = record.id")
    .SELECT(sql"WHERE slot_record.container = ${slot.containerId} AND slot_record.segment && ${slot.segment}::segment ORDER BY record.category NULLS LAST")
    .list

  /** Retrieve all the categorized records associated with the given volume.
    * @param category restrict to the specified category, or include all categories */
  def getVolume(volume : Volume, category : Option[RecordCategory] = None) : Future[Seq[Record]] =
    rowVolume(volume)
    .SELECT(category.fold(PreparedStatement(""))(c => sql"WHERE record.category = ${c.id}"))
    .list

  /** Retrieve the records in the given volume with a measure of the given value.
    * @param category restrict to the specified category, or include all categories
    * @param metric search by metric
    * @param value measure value that must match
    */
  def findMeasures[T](volume : Volume, category : Option[RecordCategory], measures : Measure[T]*) : Future[Seq[Record]] = {
    measures.zipWithIndex.foldLeft(rowVolume(volume)) { (s, mi) =>
      val (m, i) = mi
      val ma = "m_" + i.toString
      val mt = m.metric.measureType
      s.join(mt.select.column.fromAlias(ma).on(
        (("record.id = " + ma + ".record AND " + ma) +: sql".metric = ${m.metric.id} AND ") + ma + ".datum = " ++ m.sqlArg))
      .map(_._1)
    }
    .SELECT(category.fold(PreparedStatement(""))(c => sql"WHERE record.category = ${c.id}"))
    .list
  }

  /** Create a new record, initially unattached. */
  def create(volume : Volume, category : Option[RecordCategory] = None) : Future[Record] = {
    implicit val site = volume.site
    Audit.add("record", SQLTerms('volume -> volume.id, 'category -> category.map(_.id)), "id")
    .single(SQL.Cols[Id].map(new Record(_, volume, category)))
  }
}
