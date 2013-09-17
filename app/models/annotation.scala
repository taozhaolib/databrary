package models

import java.sql.{Timestamp,Date}
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

/** One of a variety of types of metadata that may be attached to other objects in the system.
  * Currently annotations can be placed on any [[Container]]s and [[Asset]]s, as determined by the [[Annotated]] trait.
  * Permissions over annotations are generally determined by the permissions on the objects to which they attach.
  * The exact permissions and ownership semantics depend on the particular asset type. */
sealed abstract class Annotation protected (val id : Annotation.Id) extends TableRowId[Annotation] {
  /** The set of slots to which this annotation applies.
    * This checks permissions, so a non-empty list implies the annotation is visible to the current user. */
  def slots(implicit site : Site) : Seq[Slot] = Slot.getAnnotation(id)
}

/** A comment made by a particular user, usually only applied to exactly one object.
  * These are immutable (and unaudited), although the author may be considered to have ownership. */
final class Comment private (override val id : Comment.Id, val whoId : Account.Id, val when : Timestamp, val text : String) extends Annotation(id) with TableRowId[Comment] {
  private val _who = CachedVal[Account, Site](Account.get(whoId)(_).get)
  /** The user who made the comment.  Cached. */
  def who(implicit site : Site) : Account = _who
}

/** A set of Measures. */
final class Record private (override val id : Record.Id, val volume : Volume, val category_ : Option[RecordCategory] = None, val consent : Consent.Value = Consent.NONE) extends Annotation(id) with TableRowId[Record] with SitePage with InVolume {
  private[this] var _category = category_
  def category : Option[RecordCategory] = _category
  def categoryId = category.map(_.id)

  /** Update the given values in the database and this object in-place. */
  def change(category : Option[RecordCategory] = _category)(implicit db : Site.DB) : Unit = {
    if (category == _category)
      return
    SQL("UPDATE record SET category = {category} WHERE id = {id}").
      on('id -> id, 'category -> category.map(_.id)).execute()
    _category = category
  }

  /** A specific measure of the given type and metric. */
  def measure[T](metric : Metric[T])(implicit db : Site.DB) : Option[T] = Measure.get[T](this.id, metric)
  private[this] val _measures = CachedVal[Seq[MeasureBase], Site.DB](Measure.getRecord(this.id)(_))
  /** All measures in this record. Cached. */
  def measures(implicit db : Site.DB) : Seq[MeasureBase] = _measures
  /** Add a measure to this record. */
  def addMeasure[T](metric : Metric[T], datum : T)(implicit db : Site.DB) = Measure.add[T](this.id, metric, datum)

  /** Add or change a measure on this record.
    * This is not type safe so may generate SQL exceptions, and may invalidate measures on this object. */
  def setMeasure(metric : MetricBase, value : String)(implicit db : Site.DB) = Measure.set(this.id, metric, value)
  /** Remove a measure from this record.
    * This may invalidate measures on this object. */
  def deleteMeasure(metric : MetricBase)(implicit db : Site.DB) = Measure.delete(this.id, metric)

  private val _ident = CachedVal[Option[String], Site.DB](measure(Metric.Ident)(_))
  /** Cached version of `measure(Metric.Ident)`.
    * This may become invalid if the value is changed. */
  def ident(implicit db : Site.DB) : Option[String] = _ident

  private val _birthdate = CachedVal[Option[Date], Site.DB](measure(Metric.Birthdate)(_))
  /** Cached version of `measure(Metric.Birthdate)`.
    * This may become invalid if the value is changed. */
  def birthdate(implicit db : Site.DB) : Option[Date] = _birthdate

  private val _daterange = CachedVal[Range[Date], Site.DB] { implicit db =>
    SQL("SELECT annotation_daterange({id})").
      on('id -> id).single(scalar[Range[Date]](PGDateRange.column))
  }
  /** The range of acquisition dates covered by associated slots. Cached. */
  def daterange(implicit db : Site.DB) : Range[Date] = _daterange.normalize

  /** The range of ages as defined by `daterange - birthdate`. */
  def agerange(implicit db : Site.DB) : Option[Range[Long]] = birthdate.map(dob => daterange.map(_.getTime - dob.getTime))

  /** The age at test for a specific date, as defined by `date - birthdate`. */
  def age(date : Date)(implicit db : Site.DB) : Option[Long] = birthdate.map(date.getTime - _.getTime)

  /** Effective permission the site user has over a given metric in this record, specifically in regards to the measure datum itself.
    * Record permissions depend on volume permissions, but can be further restricted by consent levels.
    */
  def dataPermission(metric : MetricBase)(implicit site : Site) : Permission.Value =
    Permission.data(volume.permission, consent, metric.classification)

  def pageName(implicit site : Site) = ident.orElse(category.map(_.name)).getOrElse("record")
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL = controllers.routes.Record.view(id).url
}


private[models] sealed abstract class AnnotationView[R <: Annotation with TableRowId[R]](table : String) extends TableId[R](table) {
  /** Retrieve a specific annotation of the instantiated object's type by id. */
  def get(id : Id)(implicit db : Site.DB) : Option[R] =
    row.SQL("WHERE " + table + ".id = {id}").
      on('id -> id).singleOpt

  private[models] def rowVolume(vol : Volume) : Selector[R]

  /** Retrieve the set of annotations of the instantiated object's type on the given target.
    * @param all include all indirect annotations on any containers, objects, or clips contained within the given target (which may be a lot) */
  private[models] def get(target : Annotated, all : Boolean = true)(implicit db : Site.DB) : Seq[R] = {
    val j = target.annotationTable
    rowVolume(target.volume).
      SQL((if (all) 
        "JOIN " + j + "s({target}) ON " + table + ".id = " + j + "s"
      else
        "JOIN " + j + " ON " + table + ".id = annotation WHERE " + target.annotatedLevel + " = {target}")).
      on('target -> target.annotatedId).list
  }
}

/** Dummy object for providing generic [[Annotation.Id]]s */
object Annotation extends HasId[Annotation] //AnnotationView[Annotation]("annotation")

object Comment extends AnnotationView[Comment]("comment") {
  private[models] val row = Columns[
    Id,  Account.Id, Timestamp, String](
    'id, 'who,       'when,     'text) map
    { (id, whoId, when, text) => new Comment(id, whoId, when, text) }
  private[models] def rowVolume(vol : Volume) = row

  /** Retrieve the set of comments written by the specified user. */
  private[models] def getParty(user : Account)(implicit db : Site.DB) : Seq[Comment] =
    row.SQL("WHERE who = {who}").
      on('who -> user.id).list(row map { a =>
        a._who() = user
        a
      })

  /** Post a new comment on a target by the current user.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  private[models] def post(target : Annotated, text : String)(implicit site : Site) : Comment = {
    val args = SQLArgs('who -> site.identity.id, 'text -> text)
    val c = SQL("INSERT INTO " + table + " " + args.insert + " RETURNING " + row.select).
      on(args : _*).single(row)
    c._who() = site.user.get
    SQL("INSERT INTO " + target.annotationTable + " (" + target.annotatedLevel + ", annotation) VALUES ({target}, {annotation})").
      on('target -> target.annotatedId, 'annotation -> c.id).execute()
    c
  }
}

object Record extends AnnotationView[Record]("record") {
  private[models] def make(volume : Volume, category : Option[RecordCategory])(id : Id, consent : Option[Consent.Value]) =
    new Record(id, volume, category, consent.getOrElse(Consent.NONE))
  private val columns = Columns[
    Id,  Option[Consent.Value]](
    'id, SelectAs("annotation_consent(record.id)", "annotation_consent"))
  private val colsCat = columns.leftJoin(RecordCategory.row, "record.category = record_category.id")
  private[models] val row = colsCat.join(Volume.row, "record.volume = volume.id") map {
    case (rec ~ cls ~ vol) => (make(vol, cls) _).tupled(rec)
  }
  private[models] def rowVolume(vol : Volume) = colsCat map {
    case (rec ~ cls) => (make(vol, cls) _).tupled(rec)
  }
  private[models] def rowVolCat(vol : Volume, category : Option[RecordCategory] = None) =
    category.fold(rowVolume(vol))(cat => columns.map(make(vol, Some(cat)) _))

  /** Retrieve all the categorized records associated the given volume.
    * @param category restrict to the specified category, or include all categories
    * @return records sorted by category, ident */
  private[models] def getVolume(volume : Volume, category : Option[RecordCategory] = None)(implicit db : Site.DB) : Seq[Record] = {
    val metric = Metric.Ident
    val mtyp = metric.measureType
    val cols = rowVolCat(volume, category).leftJoin(mtyp.column, "record.id = " + mtyp.table + ".record") map
      { case (record ~ ident) =>
        record._ident() = ident
        record
      }
    cols.SQL("WHERE measure_text.metric = {metric} AND record.volume = {volume}",
        (if (category.isDefined) "AND record.category = {category}" else ""),
        "ORDER BY " + (if (category.isEmpty) "record.category, " else ""),
        metric.measureType.column.select + ", record.id").
      on('volume -> volume.id, 'metric -> metric.id, 'category -> category.map(_.id)).
      list
  }

  /** Create a new record, initially unattached.
    * The result should be immediately attached to a target to make it accessible. */
  def create(volume : Volume, category : Option[RecordCategory] = None)(implicit db : Site.DB) : Record = {
    val args = SQLArgs('volume -> volume.id, 'category -> category.map(_.id))
    val id = SQL("INSERT INTO record " + args.insert + " RETURNING id").
      on(args : _*).single(scalar[Id])
    new Record(id, volume, category)
  }
}


/** Objects on which annotations may be placed. */
trait Annotated extends InVolume {
  private[models] def annotatedId : IntId[_]
  private[models] def annotatedLevel : String
  private[models] def annotationTable = annotatedLevel + "_annotation"
  /** The list of comments on this object.
    * @param all include indirect comments on any contained objects
    */
  def comments(all : Boolean = true)(implicit db : Site.DB) : Seq[Comment] = Comment.get(this, all)(db)
  /** Post a new comment this object.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  def postComment(text : String)(implicit site : Site) : Comment = Comment.post(this, text)(site)
  /** The list of records on this object.
    * @param all include indirect comments on any contained objects
    */
  def records(all : Boolean = true)(implicit db : Site.DB) : Seq[Record] = Record.get(this, all)(db)
  /** The list of records and possibly measures on this object.
    * This is essentially equivalent to `this.records(false).filter(_.category == category).map(r => (r, r.measure[T](metric)))` but more efficient.
    * @param category if Some limit to the given category */
  def recordMeasures[T](category : Option[RecordCategory] = None, metric : Metric[T] = Metric.Ident)(implicit db : Site.DB) : Seq[(Record, Option[T])] = Measure.getAnnotated[T](this, category, metric)
  /** A list of record identification strings that apply to this object.
    * This is probably not a permanent solution for naming, but it's a start. */
  def idents(implicit db : Site.DB) : Seq[(String)] = recordMeasures() map {
    case (r, i) => r.category.fold("")(_.name + ':') + i.getOrElse("[" + r.id.unId.toString + ']')
  }
}
