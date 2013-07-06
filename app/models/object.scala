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

final case class ObjectFormat private (id : ObjectFormat.Id, mimetype : String, extension : Option[String], name : String, timeseries : Boolean) extends TableRowId(id.unId) {
}

object ObjectFormat extends TableViewId[ObjectFormat]("format") {
  private[models] val row = Anorm.rowMap(ObjectFormat.apply _, "format", "mimetype", "extension", "name", "timeseries")
}

final class Object private (val id : Object.Id, val format : ObjectFormat, consent_ : Consent.Value, date_ : Option[Date]) extends TableRowId(id.unId) {
  private[this] var _consent = consent_
  def consent = _consent
  private[this] var _date = date_
  def date = _date

  def change(consent : Consent.Value = _consent, date : Option[Date] = _date)(implicit site : Site) : Unit = {
    if (date == _date && consent == _consent)
      return
    Audit.SQLon(AuditAction.change, "object", "SET consent = {consent}, date = {date} WHERE id = {id}")('consent -> consent, 'date -> date, 'id -> id).execute()(site.db)
    _consent = consent
    _date = date
  }
}

object Object extends TableViewId[Object]("object JOIN format USING (format)") {
  private[models] val row = (Anorm.rowMap(Tuple3.apply[Id, Consent.Value, Option[Date]] _, "id", "consent", "date") ~ ObjectFormat.row).map({
    case ((id, consent, date) ~ format) => new Object(id, format, consent, date)
  })
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Object] =
    SQL("SELECT " + * + " FROM " + table + " WHERE id = {id}").
      on('id -> i).singleOpt(row)
}

final class StudyObject private (val obj : Object, val studyId : Study.Id, title_ : String, description_ : Option[String]) extends TableRow {
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

  private[StudyObject] val _study = CachedVal[Study, Site](Study.get(studyId)(_).get)
  def study(implicit site : Site) : Study = _study

  def permission(implicit site : Site) : Permission.Value = {
    val p = study.permission
    if (obj.consent > Consent.DEIDENTIFIED && (
      (obj.consent > Consent.SHARED && p < Permission.EDIT) 
      || site.access < Permission.DOWNLOAD))
      Permission.NONE
    else
      p
  }
}

object StudyObject extends TableView[StudyObject]("study_object JOIN (" + Object.table + ") ON (object = id)") {
  private[models] val row = (Anorm.rowMap(Tuple3.apply[Study.Id, String, Option[String]] _, "study", "title", "description") ~ Object.row).map({
    case ((study, title, description) ~ obj) => new StudyObject(obj, study, title, description)
  })
  private[this] def rowStudy(s : Study) = row map { o => o._study() = s ; o }

  private[models] def get(s : Study, o : Object.Id)(implicit db : Site.DB) : Option[StudyObject] =
    SQL("SELECT * FROM " + table + " WHERE study = {study} AND object = {object}").
      on('study -> s.id, 'object -> o).singleOpt(rowStudy(s))
  private[models] def getObjects(s : Study)(implicit db : Site.DB) =
    SQL("SELECT * FROM " + table + " WHERE study = {study}").
      on('study -> s.id).list(rowStudy(s))
}
