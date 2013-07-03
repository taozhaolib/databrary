package models

import java.sql.Date
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

final case class ObjectFormat private (id : ObjectFormat.Id, mimetype : String, extension : Option[String], name : String, timeseries : Boolean) extends TableRowId(id.unId) {
}

object ObjectFormat extends TableViewId[ObjectFormat]("format") {
  private[models] val row = Anorm.rowMap(ObjectFormat.apply _, "format", "mimetype", "extension", "name", "timeseries")
}

final class Object private (val id : Object.Id, val format : ObjectFormat, consent : Consent.Value, date_ : Date) extends TableRowId(id.unId) {
  private[this] var _date = date_
  def date = _date

  def change(date : Date = _date)(implicit site : Site) : Unit = {
    if (date == _date)
      return
    Audit.SQLon(AuditAction.change, "object", "SET date = {date} WHERE id = {id}")('date -> date, 'id -> id).execute()(site.db)
    _date = date
  }
}

object Object extends TableViewId[Object]("object JOIN object_format USING (format)") {
  private[models] val row = (Anorm.rowMap(Tuple3.apply[Id, Consent.Value, Date] _, "id", "consent", "date") ~ ObjectFormat.row).map({
    case ((id, consent, date) ~ format) => new Object(id, format, consent, date)
  })
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Object] =
    SQL("SELECT " + * + " FROM " + table + " WHERE id = {id}").
      on('id -> i).singleOpt(row)
}

final class StudyObject private (val obj : Object, val study : Study.Id, title_ : String, description_ : Option[String]) extends TableRow {
  private[this] var _title = title_
  def title = _title
  private[this] var _description = description_
  def description = _description

  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    val args = Anorm.Args('obj -> obj.id, 'study -> study, 'title -> title, 'description -> description)
    Audit.SQLon(AuditAction.change, "study_object", "SET title = {title}, description = {description} WHERE object = {obj} AND study = {study}")(args : _*).execute()(site.db)
    _title = title
    _description = description
  }
}

object StudyObject extends TableView[StudyObject]("study_object JOIN (" + Object.table + ") ON (object = id)") {
  private[models] val row = (Anorm.rowMap(Tuple3.apply[Study.Id, String, Option[String]] _, "study", "title", "description") ~ Object.row).map({
    case ((study, title, description) ~ obj) => new StudyObject(obj, study, title, description)
  })

  def get(s : Study.Id, o : Object.Id)(implicit db : Site.DB) : Option[StudyObject] =
    SQL("SELECT * FROM " + table + " WHERE study = {study} AND object = {object}").
      on('study -> s, 'object -> o).singleOpt(row)
  def getObjects(s : Study.Id)(implicit db : Site.DB) =
    SQL("SELECT * FROM " + table + " WHERE study = {study}").
      on('study -> s).list(row)
}
