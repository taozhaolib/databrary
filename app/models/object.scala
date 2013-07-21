package models

import java.sql.Date
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

object Consent extends PGEnum("consent") {
  val PUBLIC, DEIDENTIFIED, EXCERPTS, SHARED, PRIVATE = Value
}

case class ObjectFormat private[models] (id : ObjectFormat.Id, mimetype : String, extension : Option[String], name : String, timeseries : Boolean) extends TableRowId[ObjectFormat]

object ObjectFormat extends HasId[ObjectFormat]

private[models] sealed abstract class FormatView(table : String, timeseries : Boolean) extends TableViewId[ObjectFormat](table) {
  private[this] def make(id : Id, mimetype : String, extension : Option[String], name : String) =
    new ObjectFormat(id, mimetype, extension, name, timeseries)
  private[models] val row = Anorm.rowMap(make _, col("format"), col("mimetype"), col("extension"), col("name"))
}

object FileFormat extends FormatView("file_format", false)
object TimeseriesFormat extends FormatView("timeseries_format", true)


sealed abstract class Object protected (val id : Object.Id) extends TableRowId[Object] {
  def consent : Consent.Value
}

sealed class FileObject protected (override val id : FileObject.Id, val format : ObjectFormat, consent_ : Consent.Value, date_ : Option[Date]) extends Object(id) with TableRowId[FileObject] {
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

final class TimeseriesObject private (override val id : TimeseriesObject.Id, format : ObjectFormat, consent_ : Consent.Value, date_ : Option[Date]) extends FileObject(id, format, consent_, date_) with TableRowId[TimeseriesObject]

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


private[models] sealed abstract class ObjectView[R <: Object with TableRowId[R]](table : String) extends TableViewId[R](table) {
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[R]
}

object Object extends ObjectView[Object]("object") {
  private[models] val row = ???

  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Object] =
    SQL("SELECT kind FROM object WHERE id = {id}").on('id -> i).singleOpt(scalar[String]) flatMap (_ match {
      case "file" => FileObject.get(FileObject.asId(i.unId))
      case "timeseries" => TimeseriesObject.get(TimeseriesObject.asId(i.unId))
      case "excerpt" => Excerpt.get(Excerpt.asId(i.unId))
    })
}

object FileObject extends ObjectView[FileObject]("file") {
  private[models] val baseRow = Anorm.rowMap(Tuple3.apply[Id, Consent.Value, Option[Date]] _, col("id"), col("consent"), col("date"))
  private[models] val row = (baseRow ~ FileFormat.row) map {
    case ((id, consent, date) ~ format) => new FileObject(id, format, consent, date)
  }
  private[models] override val src = "ONLY file JOIN ONLY file_format ON file.format = file_format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[FileObject] =
    SQL("SELECT " + * + " FROM " + src + " WHERE file.id = {id}").
      on('id -> i).singleOpt(row)
}

object TimeseriesObject extends ObjectView[TimeseriesObject]("timeseries") {
  private[models] val baseRow = Anorm.rowMap(Tuple3.apply[Id, Consent.Value, Option[Date]] _, col("id"), col("consent"), col("date"))
  private[models] val row = (baseRow ~ TimeseriesFormat.row) map {
    case ((id, consent, date) ~ format) => new TimeseriesObject(id, format, consent, date)
  }
  private[models] override val src = "timeseries JOIN timeseries_format ON timeseries.format = timeseries_format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[TimeseriesObject] =
    SQL("SELECT " + * + " FROM " + src + " WHERE timeseries.id = {id}").
      on('id -> i).singleOpt(row)
}

object Excerpt extends ObjectView[Excerpt]("excerpt") {
  private[this] val baseRow = Anorm.rowMap(Tuple2.apply[Id, Boolean] _, col("id"), col("public"))
  private[this] def baseMake(ii : (Id, Boolean), source : TimeseriesObject) = new Excerpt(ii._1, source, ii._2)
  private[models] override val * = col("id", "public")

  private[models] val row = 
    (baseRow ~ TimeseriesObject.row) map {
      case (base ~ source) => baseMake(base, source)
    }
  private[models] override val src = "excerpt JOIN " + TimeseriesObject.src + " ON excerpt.source = timeseries.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Excerpt] =
    SQL("SELECT " + * + ", " + TimeseriesObject.* + " FROM " + src + " WHERE excerpt.id = {id}").
      on('id -> i).singleOpt(row)
}

