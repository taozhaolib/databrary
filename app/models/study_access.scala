package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

final case class StudyAccess(studyId : Study.Id, partyId : Party.Id, access : Permission.Value, inherit : Permission.Value) extends TableRow {
  private def id =
    Anorm.Args('study -> studyId, 'party -> partyId)
  private def args = 
    id ++ Anorm.Args('access -> access, 'inherit -> inherit)

  def set(implicit site : Site) : Unit = {
    val args = this.args
    if (Audit.SQLon(AuditAction.change, "study_access", "SET access = {access}, inherit = {inherit} WHERE study = {study} AND party = {party}")(args : _*).executeUpdate()(site.db) == 0)
      Audit.SQLon(AuditAction.add, "study_access", Anorm.insertArgs(args))(args : _*).execute()(site.db)
  }
  def remove(implicit site : Site) : Unit =
    StudyAccess.delete(studyId, partyId)

  private val _study = CachedVal[Study, Site](Study.get(studyId)(_).get)
  def study(implicit site : Site) : Study = _study
  private val _party = CachedVal[Party, Site](Party.get(partyId)(_).get)
  def party(implicit site : Site) : Party = _party
}

object StudyAccess extends Table[StudyAccess]("study_access") {
  private[models] val row = Columns[
    Study.Id, Party.Id, Permission.Value, Permission.Value](
    'study,   'party,   'access,          'inherit).
    map(StudyAccess.apply _)

  def get(s : Study.Id, e : Party.Id)(implicit db : Site.DB) : Option[StudyAccess] =
    SELECT("WHERE study = {study} AND party = {party}").
      on('study -> s, 'party -> e).singleOpt()

  private[models] def getEntities(s : Study, p : Permission.Value = Permission.NONE)(implicit db : Site.DB) =
    JOIN(Party, "ON (party = id) WHERE study = {study} AND access >= {access} ORDER BY access DESC").
      on('study -> s.id, 'access -> p).list((row ~ Party.row).
        map({ case (a ~ e) => a._party() = e; a._study() = s; a })
      )
  private[models] def getStudies(e : Party, p : Permission.Value = Permission.NONE)(implicit site : Site) =
    JOIN(Study, "ON (study = id) WHERE party = {party} AND access >= {access} AND " + Study.condition + " ORDER BY access DESC").
      on('party -> e.id, 'access -> p, 'identity -> site.identity.id).list((row ~ Study.row).
        map({ case (a ~ s) => a._study() = s; a._party() = e; a })
      )(site.db)

  def delete(s : Study.Id, e : Party.Id)(implicit site : Site) =
    Audit.SQLon(AuditAction.remove, "study_access", "WHERE study = {study} AND party = {party}")('study -> s, 'party -> e).
      execute()(site.db)

  def check(s : Study.Id, e : Party.Id)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT study_access_check({study}, {party})").
      on('study -> s, 'party -> e).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)
}

