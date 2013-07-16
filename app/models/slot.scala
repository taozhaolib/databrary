package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

final class Slot private (val id : Slot.Id, val studyId : Study.Id, ident_ : String) extends TableRowId(id.unId) {
  private[this] var _ident = ident_
  def ident = _ident

  def change(ident : String = _ident)(implicit site : Site) : Unit = {
    if (ident == _ident)
      return
    val args = Anorm.Args('id -> id, 'ident -> ident)
    /* TODO: catch unique violation */
    Audit.SQLon(AuditAction.change, "slot", "SET ident = {ident} WHERE id = {id}")(args : _*).execute()(site.db)
    _ident = ident
  }

  private val _study = CachedVal[Study, Site](Study.get(studyId)(_).get)
  def study(implicit site : Site) : Study = _study
}

object Slot extends TableViewId[Slot]("slot") {
  private[this] def make(id : Id, studyId : Study.Id, ident : String) =
    new Slot(id, studyId, ident)
  private[models] val row = Anorm.rowMap(make _, "id", "study", "ident")
  private[this] def rowStudy(s : Study) = row map { l => l._study() = s ; l }

  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Slot] =
    SQL("SELECT " + * + " FROM " + table + " WHERE id = {id}").
      on('id -> i).singleOpt(row)
  private[models] def getStudy(study : Study)(implicit db : Site.DB) : Seq[Slot] =
    SQL("SELECT " + * + " FROM " + table + " WHERE study = {study} ORDER BY ident").
      on('study -> study.id).list(rowStudy(study))
    
  private[models] def create(study : Study, ident : String)(implicit site : Site) : Slot = {
    val args = Anorm.Args('study -> study, 'ident -> ident)
    Audit.SQLon(AuditAction.add, table, Anorm.insertArgs(args), *)(args : _*).single(rowStudy(study))(site.db)
  }
}
