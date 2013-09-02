package models

import java.sql.Timestamp
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
  /** The set of containers to which this annotation applies.
    * This checks permissions, so a non-empty list implies the annotation is visible to the current user. */
  def containers(implicit site : Site) : Seq[Container] = Container.getAnnotation(this)(site)
  /** The set of assets to which this annotation applies.
    * This does not check permissions, so must be followed by additional checks such as `assets.flatMap(_.containers)` to ensure the annotation (and asset) may be accessed. */
  def assets(implicit db : Site.DB) : Seq[Asset] = Asset.getAnnotation(this)(db)

  /** The set of all containers to which this asset applies, directly or indirectly through contained assets or clips of those assets.
    * This does check permissions, but is not necessarily very useful, because it does not indicate exactly which objects the annotation is applied to. */
  def allContainers(implicit site : Site) : Seq[Container] = 
    containers(site) ++ assets(site.db).flatMap(_.containers(true)(site).map(_.container(site)))
}

/** A comment made by a particular user, usually only applied to exactly one object.
  * These are immutable (and unaudited), although the author may be considered to have ownership. */
final class Comment private (override val id : Comment.Id, val whoId : Account.Id, val when : Timestamp, val text : String) extends Annotation(id) with TableRowId[Comment] {
  private val _who = CachedVal[Account, Site](Account.get(whoId)(_).get)
  /** The user who made the comment. */
  def who(implicit site : Site) : Account = _who
}

/** A set of Measures. */
final class Record private (override val id : Record.Id, val category_ : Option[RecordCategory] = None) extends Annotation(id) with TableRowId[Record] {
  private[this] var _category = category_
  def category = _category
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
  def measure[T](metric : Metric[T])(implicit db : Site.DB) : Option[T] = Measure.get[T](this.id, metric)(db)
  /** All measures in this record. */
  def measures(implicit db : Site.DB) : Seq[MeasureBase] = Measure.getRecord(this.id)(db)
  /** Add a measure to this record. */
  def addMeasure[T](metric : Metric[T], datum : T)(implicit db : Site.DB) = Measure.add[T](this.id, metric, datum)(db)

  private val _ident = CachedVal[Option[String], Site.DB](measure(Metric.Ident)(_))
  /** Cached version of `measure(Metric.Ident)`.
    * Unfortunately, this may become invalid if ident is changed. */
  def ident = _ident
}


private[models] sealed abstract class AnnotationView[R <: Annotation with TableRowId[R]](table : String) extends TableId[R](table) {
  /** Retrieve a specific annotation of the instantiated object's type by id. */
  def get(id : Id)(implicit db : Site.DB) : Option[R] =
    SELECT("WHERE " + table + ".id = {id}").
      on('id -> id).singleOpt()

  /** Retrieve the set of annotations of the instantiated object's type on the given target.
    * @param all include all indirect annotations on any containers, objects, or clips contained within the given target (which may be a lot) */
  private[models] def get(target : Annotated, all : Boolean = true)(implicit db : Site.DB) : Seq[R] = {
    val j = target.annotatedLevel + "_annotation"
    SELECT(if (all) 
        "JOIN " + j + "s({target}) ON " + table + ".id = " + j + "s"
      else
        "JOIN " + j + " ON " + table + ".id = annotation WHERE " + target.annotatedLevel + " = {target}").
      on('target -> target.annotatedId).list()
  }
}

/** Dummy object for providing generic [[Annotation.Id]]s */
object Annotation extends HasId[Annotation] //AnnotationView[Annotation]("annotation")

object Comment extends AnnotationView[Comment]("comment") {
  private[models] val row = Columns[
    Id,  Account.Id, Timestamp, String](
    'id, 'who,       'when,     'text).map {
    (id, whoId, when, text) => new Comment(id, whoId, when, text)
  }

  /** Retrieve the set of comments written by the specified user. */
  private[models] def getParty(user : Account)(implicit db : Site.DB) : Seq[Comment] =
    SELECT("WHERE who = {who}").
      on('who -> user.id).list(row map { a =>
        a._who() = user
        a
      })

  /** Post a new comment on a target by the current user.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  private[models] def post(target : Annotated, text : String)(implicit site : Site) : Comment = {
    val args = SQLArgs('who -> site.identity.id, 'text -> text)
    val c = SQL("INSERT INTO " + table + " " + args.insert + " RETURNING " + *).
      on(args : _*).single(row)(site.db)
    c._who() = site.user.get
    Audit.add(target.annotatedLevel + "_annotation", SQLArgs(Symbol(target.annotatedLevel) -> target.annotatedId, 'annotation -> c.id)).
      execute()(site.db)
    c
  }
}

object Record extends AnnotationView[Record]("record") {
  private[this] val columns = Columns[
    Id](
    'id)
  private[models] val row = (columns ~ RecordCategory.row.?) map {
    case (id ~ cls) => new Record(id, cls)
  }
  private[models] override val src = "record LEFT JOIN record_category ON record.category = record_category.id"

  /** Retrieve all the categorized records associated with slots in the given study.
    * @param category restrict to the specified category, or include all categories
    * @return unique records sorted by category, ident */
  private[models] def getSlots(study : Study, category : Option[RecordCategory] = None)(implicit db : Site.DB) : Seq[Record] = {
    val metric = Metric.Ident
    val cols = (category.fold(row)(cat => columns.map(new Record(_, Some(cat)))) ~ metric.measureType.column.?) map 
      { case (record ~ ident) => record._ident() = ident; record }
    SQL(
      "SELECT " + cols.select + " FROM record" + 
        (if (category.isEmpty) " JOIN record_category ON category = record_category.id" else "") + 
        " JOIN container_annotation ON record.id = annotation JOIN slot ON container = slot.id LEFT JOIN measure_text ON record.id = " + metric.measureType.table + ".record WHERE slot.study = {study} AND measure_text.metric = {metric}" +
        (if (category.isDefined) " AND category = {category}" else "") +
        " GROUP BY " + cols.select + " ORDER BY" + 
        (if (category.isEmpty) " record_category.id," else "") +
        " " + metric.measureType.column + ", record.id").
      on('study -> study.id, 'ident -> metric.id, 'category -> category.map(_.id)).
      list(cols)
  }

  /** Create a new record, initially unattached.
    * The result should be immediately attached to a target to make it accessible. */
  def create(category : Option[RecordCategory] = None)(implicit db : Site.DB) : Record = {
    val id = SQL("INSERT INTO record (category) VALUES ({category}) RETURNING id").
      on('category -> category.map(_.id)).single(scalar[Id])
    new Record(id, category)
  }
}


/** Objects on which annotations may be placed. */
trait Annotated {
  private[models] def annotatedId : IntId[_]
  private[models] def annotatedLevel : String
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
}
