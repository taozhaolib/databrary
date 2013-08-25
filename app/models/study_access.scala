package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

/** Access control over a study granted to a party.
  * Study access permissions (ACLs) are determined by a set of StudyAccess entries (ACEs).
  * Note that study access can also be granted to descendents ("members") of the assigned party in one of two ways: through explicit inheritance within StudyAccess, or through delegated permissions within an Authorization.
  * Only one of these is necessary to gain access (they are unioned, not intersected).
  *
  * Unlike most [TableRow] classes, these may be constructed directly, and thus are not expected to reflect the current state of the database in the same way and have different update semantics.
  * @constructor create an access object, not (yet) persisted to the database
  * @param studyId the study to which access is being granted; the target
  * @param partyId the party being granted the access; the user
  * @param access the level of permission granted directly to the party. Levels at or above [[Permission.EDIT]] are considered study "membership."
  * @param inherit the level of permission granted to all descendents/members of the party, which cannot be [[Permission.ADMIN]]
  */
final case class StudyAccess(studyId : Study.Id, partyId : Party.Id, access : Permission.Value, inherit : Permission.Value) extends TableRow {
  /** Update or add this access in the database.
    * If an access for the study and party already exist, it is changed to match this.
    * Otherwise, a new one is added.
    * This may invalidate study.access. */
  def set(implicit site : Site) : Unit = {
    val id = SQLArgs('study -> studyId, 'party -> partyId)
    val args =  SQLArgs('access -> access, 'inherit -> inherit)
    if (Audit.change("study_access", args, id).executeUpdate()(site.db) == 0)
      Audit.add("study_access", args ++ id).execute()(site.db)
  }
  /** Remove this access from the database.
    * Only study and party are relevant for this operation.
    * This may invalidate study.access. */
  def remove(implicit site : Site) : Unit =
    StudyAccess.delete(studyId, partyId)

  private val _study = CachedVal[Study, Site](Study.get(studyId)(_).get)
  /** The study to which access is being granted.
    * If the current user does not have access to study, this may throw an exception. */
  def study(implicit site : Site) : Study = _study
  private val _party = CachedVal[Party, Site](Party.get(partyId)(_).get)
  /** The party being granted access. */
  def party(implicit site : Site) : Party = _party
}

object StudyAccess extends Table[StudyAccess]("study_access") {
  private[models] val row = Columns[
    Study.Id, Party.Id, Permission.Value, Permission.Value](
    'study,   'party,   'access,          'inherit).
    map(StudyAccess.apply _)

  /** Retrieve a specific study access identified by study and party.
    * This does not check for appropriate permissions on study, so is unsafe. */
  private[models] def get(study : Study.Id, party : Party.Id)(implicit db : Site.DB) : Option[StudyAccess] =
    SELECT("WHERE study = {study} AND party = {party}").
      on('study -> study, 'party -> party).singleOpt()

  /** Retrieve the access entries for a study at or above the specified permission level. */
  private[models] def getParties(study : Study, permission : Permission.Value = Permission.NONE)(implicit db : Site.DB) =
    JOIN(Party, "ON (party = id) WHERE study = {study} AND access >= {access} ORDER BY access DESC").
      on('study -> study.id, 'access -> permission).list((row ~ Party.row).
        map({ case (a ~ e) => a._party() = e; a._study() = study; a })
      )
  /** Retrieve the study access entries granted to a party at or above the specified permission level. */ 
  private[models] def getStudies(party : Party, permission : Permission.Value = Permission.NONE)(implicit site : Site) =
    JOIN(Study, "ON (study = id) WHERE party = {party} AND access >= {access} AND " + Study.condition + " ORDER BY access DESC").
      on('party -> party.id, 'access -> permission, 'identity -> site.identity.id).list((row ~ Study.row).
        map({ case (a ~ s) => a._study() = s; a._party() = party; a })
      )(site.db)

  /** Remove a particular study access from the database.
    * @return true if a matching study access was found and deleted
    */
  def delete(study : Study.Id, party : Party.Id)(implicit site : Site) =
    Audit.remove("study_access", SQLArgs('study -> study, 'party -> party)).
      execute()(site.db)

  /** Determine what permission level the party has over the study.
    * This takes into account full permission semantics including inheritance.
    * It purposefully takes a study id which may not yet have been checked for permissions. */
  def check(study : Study.Id, party : Party.Id)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT study_access_check({study}, {party})").
      on('study -> study, 'party -> party).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)
}

