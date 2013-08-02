package models

import java.sql.Date
import play.api.libs.Files.TemporaryFile
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

object Consent extends PGEnum("consent") {
  val PUBLIC, DEIDENTIFIED, EXCERPTS, SHARED, PRIVATE = Value
}

case class ObjectFormat private[models] (id : ObjectFormat.Id, mimetype : String, extension : Option[String], name : String, timeseries : Boolean) extends TableRowId[ObjectFormat]

private[models] sealed abstract class FormatView(table : String, timeseries : Boolean) extends TableColumnsId3[
    ObjectFormat, String,     Option[String], String](
    table,        "mimetype", "extension",    "name") {
  private[this] def make(id : Id, mimetype : String, extension : Option[String], name : String) =
    new ObjectFormat(id, mimetype, extension, name, timeseries)
  private[models] val row = columns.map(make _)
}

object ObjectFormat extends TableColumnsId4[
    ObjectFormat, String,     Option[String], String, Long](
    "format",     "mimetype", "extension",    "name", "tableoid") {
  private[this] def make(id : Id, mimetype : String, extension : Option[String], name : String, tableoid : Long)(implicit db : Site.DB) =
    new ObjectFormat(id, mimetype, extension, name, tableoid == TimeseriesFormat.tableOID)
  private[this] def row(implicit db : Site.DB) = columns.map(make _)

  def getMimetype(mimetype : String)(implicit db : Site.DB) : Option[ObjectFormat] =
    SELECT("WHERE mimetype = {mimetype}").
      on('mimetype -> mimetype).singleOpt(row(db))
  def getAll(implicit db : Site.DB) : Seq[ObjectFormat] =
    SELECT("ORDER BY " + col("id")).list(row(db))
}

object FileFormat extends FormatView("file_format", false)
object TimeseriesFormat extends FormatView("timeseries_format", true)


sealed abstract class Object protected (val id : Object.Id) extends TableRowId[Object] {
  def consent : Consent.Value
}

sealed class FileObject protected (override val id : FileObject.Id, val format : ObjectFormat, val ownerId : Option[Study.Id], consent_ : Consent.Value, date_ : Option[Date]) extends Object(id) with TableRowId[FileObject] {
  private[this] var _consent = consent_
  def consent = _consent
  private[this] var _date = date_
  def date = _date

  def change(consent : Consent.Value = _consent, date : Option[Date] = _date)(implicit site : Site) : Unit = {
    if (date == _date && consent == _consent)
      return
    Audit.SQLon(AuditAction.change, "file", "SET consent = {consent}, date = {date} WHERE id = {id}")('consent -> consent, 'date -> date, 'id -> id).execute()(site.db)
    _consent = consent
    _date = date
  }
}

final class TimeseriesObject private (override val id : TimeseriesObject.Id, format : ObjectFormat, ownerId : Option[Study.Id], consent_ : Consent.Value, date_ : Option[Date], val length : Interval) extends FileObject(id, format, ownerId, consent_, date_) with TableRowId[TimeseriesObject]

final class Excerpt private (override val id : Excerpt.Id, val source : TimeseriesObject, public_ : Boolean) extends Object(id) with TableRowId[Excerpt] {
  def sourceId = source.id
  private[this] var _public = public_
  def public = _public

  def change(public : Boolean = _public)(implicit site : Site) : Unit = {
    if (public == _public)
      return
    Audit.SQLon(AuditAction.change, "excerpt", "SET public = {public} WHERE id = {id}")('public -> public, 'id -> id).execute()(site.db)
    _public = public
  }

  def consent = {
    val c = source.consent
    if (c <= Consent.EXCERPTS && public)
      Consent.PUBLIC
    else
      c
  }
}


private[models] sealed abstract trait ObjectView[R <: Object with TableRowId[R]] extends TableView[R] with HasId[R] {
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[R]
}

object Object extends TableColumns1[Object, String]("object", "kind") with ObjectView[Object] {
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Object] =
    SELECT("WHERE id = {id}").on('id -> i).singleOpt(scalar[String]) flatMap (_ match {
      case "file" => FileObject.get(FileObject.asId(i.unId))
      case "timeseries" => TimeseriesObject.get(TimeseriesObject.asId(i.unId))
      case "excerpt" => Excerpt.get(Excerpt.asId(i.unId))
    })
}

object FileObject extends TableColumnsId3[
      FileObject, Option[Study.Id], Consent.Value, Option[Date]](
      "file",     "owner",          "consent",     "date")
    with ObjectView[FileObject] {
  private[models] val row = (columns ~ FileFormat.row) map {
    case ((id, owner, consent, date) ~ format) => new FileObject(id, format, owner, consent, date)
  }
  private[models] override val * = columns.select + ", " + FileFormat.*
  private[models] override val src = "ONLY file JOIN ONLY file_format ON file.format = file_format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[FileObject] =
    SELECT("WHERE file.id = {id}").
      on('id -> i).singleOpt(row)

  def create(format : ObjectFormat, owner : Study.Id, consent : Consent.Value, date : Option[Date], file : TemporaryFile)(implicit site : Site) : FileObject = {
    val args = Anorm.Args('format -> format.ensuring(!_.timeseries).id, 'owner -> owner, 'consent -> consent, 'date -> date)
    val id = Audit.SQLon(AuditAction.add, table, Anorm.insertArgs(args), "id")(args : _*).single(scalar[Id])(site.db)
    store.Object.store(id, file)
    site.db.commit
    new FileObject(id, format, Some(owner), consent, date)
  }
}

object TimeseriesObject extends TableColumnsId4[
      TimeseriesObject, Option[Study.Id], Consent.Value, Option[Date], Interval](
      "timeseries",     "owner",          "consent",     "date",       "length")
    with ObjectView[TimeseriesObject] {
  private[models] val row = (columns ~ TimeseriesFormat.row) map {
    case ((id, owner, consent, date, length) ~ format) => new TimeseriesObject(id, format, owner, consent, date, length)
  }
  private[models] override val * = columns.select + ", " + TimeseriesFormat.*
  private[models] override val src = "timeseries JOIN timeseries_format ON timeseries.format = timeseries_format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[TimeseriesObject] =
    SELECT("WHERE timeseries.id = {id}").
      on('id -> i).singleOpt(row)
}

object Excerpt extends TableColumnsId1[
      Excerpt,   Boolean](
      "excerpt", "public")
    with ObjectView[Excerpt] {
  private[this] def baseMake(ii : (Id, Boolean), source : TimeseriesObject) = new Excerpt(ii._1, source, ii._2)

  private[models] val row = 
    (columns ~ TimeseriesObject.row) map {
      case (base ~ source) => baseMake(base, source)
    }
  private[models] override val * = columns.select + ", " + TimeseriesObject.*
  private[models] override val src = "excerpt JOIN " + TimeseriesObject.src + " ON excerpt.source = timeseries.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Excerpt] =
    SELECT("WHERE excerpt.id = {id}").
      on('id -> i).singleOpt(row)
}

