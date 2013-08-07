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

private[models] sealed abstract class FormatView(table : String, timeseries : Boolean) extends TableId[ObjectFormat](table) {
  private[models] val row = Columns[
    Id,  String,    Option[String], String](
    'id, 'mimetype, 'extension,     'name) map {
    (id, mimetype, extension, name) => new ObjectFormat(id, mimetype, extension, name, timeseries)
  }
}

object ObjectFormat extends TableView with HasId[ObjectFormat] {
  private[models] val table = "object"
  private[this] val columns = Columns[
    Id,  String,    Option[String], String, Long](
    'id, 'mimetype, 'extension,     'name,  'tableoid)
  private[this] def row(implicit db : Site.DB) = columns map {
    (id : Id, mimetype : String, extension : Option[String], name : String, tableoid : Long) => new ObjectFormat(id, mimetype, extension, name, tableoid == TimeseriesFormat.tableOID)
  }
  /* unfortunate for TableView: */
  type Row = (Id, String, Option[String], String, Long)
  private[models] val row = columns

  def getMimetype(mimetype : String)(implicit db : Site.DB) : Option[ObjectFormat] =
    SELECT("WHERE mimetype = {mimetype}").
      on('mimetype -> mimetype).singleOpt(row)
  def getAll(implicit db : Site.DB) : Seq[ObjectFormat] =
    SELECT("ORDER BY object.id").list(row)
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

final class TimeseriesObject private (override val id : TimeseriesObject.Id, format : ObjectFormat, ownerId : Option[Study.Id], consent_ : Consent.Value, date_ : Option[Date], val duration : Interval) extends FileObject(id, format, ownerId, consent_, date_) with TableRowId[TimeseriesObject]

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


private[models] sealed abstract trait ObjectView[R <: Object with TableRowId[R]] extends TableView with HasId[R] {
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[R]
}

object Object extends ObjectView[Object] {
  private[models] val table = "object"
  type Row = String
  private[models] val row = Columns[String]('kind)
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Object] =
    SELECT("WHERE id = {id}").on('id -> i).singleOpt() flatMap {
      case "file" => FileObject.get(FileObject.asId(i.unId))
      case "timeseries" => TimeseriesObject.get(TimeseriesObject.asId(i.unId))
      case "excerpt" => Excerpt.get(Excerpt.asId(i.unId))
    }

  def create(format : ObjectFormat, owner : Study.Id, consent : Consent.Value, date : Option[Date], file : TemporaryFile)(implicit site : Site) : FileObject = {
    val obj = if (format.timeseries)
        TimeseriesObject.create(format, owner, consent, date, file)
      else
        FileObject.create(format, owner, consent, date, file)
    store.Object.store(obj.id, file)
    site.db.commit
    obj
  }
}

object FileObject extends TableId[FileObject]("file") with ObjectView[FileObject] {
  private[this] val columns = Columns[
    Id,  Option[Study.Id], Consent.Value, Option[Date]](
    'id, 'owner,           'consent,      'date)
  private[models] val row = (columns ~ FileFormat.row) map {
    case ((id, owner, consent, date) ~ format) => new FileObject(id, format, owner, consent, date)
  }
  private[models] override val src = "ONLY file JOIN ONLY file_format ON file.format = file_format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[FileObject] =
    SELECT("WHERE file.id = {id}").
      on('id -> i).singleOpt()

  private[models] def create(format : ObjectFormat, owner : Study.Id, consent : Consent.Value, date : Option[Date], file : TemporaryFile)(implicit site : Site) : FileObject = {
    val args = Anorm.Args('format -> format.ensuring(!_.timeseries).id, 'owner -> owner, 'consent -> consent, 'date -> date)
    val id = Audit.SQLon(AuditAction.add, table, Anorm.insertArgs(args), "id")(args : _*).single(scalar[Id])(site.db)
    new FileObject(id, format, Some(owner), consent, date)
  }
}

object TimeseriesObject extends TableId[TimeseriesObject]("timeseries") with ObjectView[TimeseriesObject] {
  private[this] val columns = Columns[
    Id,  Option[Study.Id], Consent.Value, Option[Date], Interval](
    'id, 'owner,           'consent,      'date,        'duration)
  private[models] val row = (columns ~ TimeseriesFormat.row) map {
    case ((id, owner, consent, date, duration) ~ format) => new TimeseriesObject(id, format, owner, consent, date, duration)
  }
  private[models] override val src = "timeseries JOIN timeseries_format ON timeseries.format = timeseries_format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[TimeseriesObject] =
    SELECT("WHERE timeseries.id = {id}").
      on('id -> i).singleOpt()

  private[models] def create(format : ObjectFormat, owner : Study.Id, consent : Consent.Value, date : Option[Date], file : TemporaryFile)(implicit site : Site) : FileObject = {
    val fmt = media.AV.probe(file.file.getPath)
    val duration = Interval(fmt.duration)
    val args = Anorm.Args('format -> format.ensuring(_.timeseries).id, 'owner -> owner, 'consent -> consent, 'date -> date, 'duration -> duration)
    val id = Audit.SQLon(AuditAction.add, table, Anorm.insertArgs(args), "id")(args : _*).single(scalar[Id])(site.db)
    new TimeseriesObject(id, format, Some(owner), consent, date, duration)
  }
}

object Excerpt extends TableId[Excerpt]("excerpt") with ObjectView[Excerpt] {
  private[this] val columns = Columns[
    Id,  Boolean](
    'id, 'public)
  private[models] val row = (columns ~ TimeseriesObject.row) map {
    case ((id, public) ~ source) => new Excerpt(id, source, public)
  }
  private[models] override val src = "excerpt JOIN " + TimeseriesObject.src + " ON excerpt.source = timeseries.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Excerpt] =
    SELECT("WHERE excerpt.id = {id}").
      on('id -> i).singleOpt()
}

