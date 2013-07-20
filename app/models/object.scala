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

case class ObjectFormat private[models] (id : ObjectFormat.Id, mimetype : String, extension : Option[String], name : String, timeseries : Boolean) extends TableRowId(id.unId)

object ObjectFormat extends NewId

private[models] sealed abstract class FormatView(table : String, timeseries : Boolean) extends TableView[ObjectFormat](table) with HasId {
  type Id = ObjectFormat.Id
  def asId(i : Int) : Id = ObjectFormat.asId(i)

  private[this] def make(id : Id, mimetype : String, extension : Option[String], name : String) =
    new ObjectFormat(id, mimetype, extension, name, timeseries)
  private[models] val row = Anorm.rowMap(make _, "format", "mimetype", "extension", "name")
}

object FileFormat extends FormatView("file_format", false)
object TimeseriesFormat extends FormatView("timeseries_format", true)


sealed abstract class Object protected (val id : Object.Id) extends TableRowId(id.unId) {
  def consent : Consent.Value
}

sealed class FileObject protected (id : FileObject.Id, val format : ObjectFormat, consent_ : Consent.Value, date_ : Option[Date]) extends Object(id) {
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

final class TimeseriesObject private (id : TimeseriesObject.Id, format : ObjectFormat, consent_ : Consent.Value, date_ : Option[Date]) extends FileObject(id, format, consent_, date_)

final class Excerpt private (id : Excerpt.Id, val sourceId : TimeseriesObject.Id, public_ : Boolean) extends Object(id) {
  private[this] var _public = public_
  def public = _public

  def change(public : Boolean = _public)(implicit site : Site) : Unit = {
    if (public == _public)
      return
    Audit.SQLon(AuditAction.change, "excerpt", "SET public = {public} WHERE id = {id}")('public -> public, 'id -> id).execute()(site.db)
    _public = public
  }
}


private[models] object ObjectId extends NewId

private[models] sealed abstract class ObjectView[R <: Object](table : String) extends TableView[R](table) with HasId {
  type Id = ObjectId.Id
  def asId(i : Int) : Id = ObjectId.asId(i)

  def get(i : Id)(implicit db : Site.DB) : Option[R]
}

object Object extends ObjectView[Object]("object") {
}

object FileObject extends ObjectView[FileObject]("file") {
  private[models] val baseRow = Anorm.rowMap(Tuple3.apply[Id, Consent.Value, Option[Date]] _, col("id"), col("consent"), col("date"))
  private[models] val row = (baseRow ~ FileFormat.row) map {
    case ((id, consent, date) ~ format) => new FileObject(id, format, consent, date)
  }
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[FileObject] =
    SQL("SELECT " + * + " FROM file JOIN file_format ON file.format = file_format.id WHERE id = {id}").
      on('id -> i).singleOpt(row)
}

object TimeseriesObject extends ObjectView[TimeseriesObject]("timeseries") {
  private[models] val baseRow = FileObject.baseRow
  private[models] val row = (baseRow ~ TimeseriesFormat.row) map {
    case ((id, consent, date) ~ format) => new TimeseriesObject(id, format, consent, date)
  }
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[TimeseriesObject] =
    SQL("SELECT " + * + " FROM timeseries JOIN timeseries_format ON timeseries.format = timeseries_format.id WHERE id = {id}").
      on('id -> i).singleOpt(row)
}

object Excerpt extends ObjectView[Excerpt]("excerpt") {
  private[this] def make(id : Id, sourceId : TimeseriesObject.Id, public : Boolean) =
    new Excerpt(id, sourceId, public)
  private[models] val row = Anorm.rowMap(make _, col("id"), col("source"), col("public"))
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Excerpt] =
    SQL("SELECT " + * + " FROM excerpt WHERE id = {id}").
      on('id -> i).singleOpt(row)
}

