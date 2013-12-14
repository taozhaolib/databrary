package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import site._

/** Access control over a volume granted to a party.
  * Volume access permissions (ACLs) are determined by a set of VolumeAccess entries (ACEs).
  * Note that volume access can also be granted to descendents ("members") of the assigned party in one of two ways: through explicit inheritance within VolumeAccess, or through delegated permissions within an Authorization.
  * Only one of these is necessary to gain access (they are unioned, not intersected).
  *
  * Unlike most [TableRow] classes, these may be constructed directly, and thus are not expected to reflect the current state of the database in the same way and have different update semantics.
  * @constructor create an access object, not (yet) persisted to the database
  * @param volume the volume to which access is being granted; the target
  * @param party the party being granted the access; the user
  * @param access the level of permission granted directly to the party. Levels at or above [[Permission.EDIT]] are considered volume "membership."
  * @param inherit the level of permission granted to all descendents/members of the party, which cannot be [[Permission.ADMIN]]
  */
final class VolumeAccess(val volume : Volume, val party : Party, val access : Permission.Value, val inherit : Permission.Value) extends TableRow with InVolume {
  def partyId = party.id

  lazy val json = JsonObject(
    'volume -> volume.json,
    'party -> party.json,
    'access -> access
  )
}

object VolumeAccess extends Table[VolumeAccess]("volume_access") {
  private val columns = Columns(
      SelectColumn[Permission.Value]("access")
    , SelectColumn[Permission.Value]("inherit")
    ).map { (access, inherit) =>
      (volume : Volume, party : Party) => new VolumeAccess(volume, party, access, inherit)
    }
  private def partyRow(party : Party)(implicit site : Site) =
    columns.join(Volume.row, "volume_access.volume = volume.id") map {
      case (a, vol) => a(vol, party)
    }
  private def volumeRow(volume : Volume) =
    columns.join(Party.row, "volume_access.party = party.id") map {
      case (a, who) => a(volume, who)
    }

  /** Retrieve the access entries for a volume. */
  private[models] def getParties(volume : Volume) : Future[Seq[VolumeAccess]] =
    volumeRow(volume)
      .SELECT("WHERE volume = ? ORDER BY access DESC, party.name")
      .apply(volume.id).list
  /** Retrieve the volume access entries granted to a party at or above the specified permission level. */ 
  private[models] def getVolumes(party : Party, permission : Permission.Value = Permission.NONE)(implicit site : Site) : Future[Seq[VolumeAccess]] =
    partyRow(party)
      .SELECT("WHERE party = ? AND access >= ?::permission AND", Volume.condition, "ORDER BY access DESC, volume.name")
      .apply(SQLArgs(party.id, permission) ++ Volume.conditionArgs).list

  /** Update or add volume access in the database.
    * If an access for the volume and party already exist, it is changed to match this.
    * Otherwise, a new one is added.
    * This may invalidate volume.access. */
  def set(volume : Volume, party : Party.Id, access : Permission.Value, inherit : Permission.Value)(implicit site : Site) : Future[Boolean] =
    Audit.changeOrAdd("volume_access", SQLTerms('access -> access, 'inherit -> inherit), SQLTerms('volume -> volume.id, 'party -> party)).execute
  /** Remove a particular volume access from the database.
    * @return true if a matching volume access was found and deleted
    */
  def delete(volume : Volume, party : Party.Id)(implicit site : Site) : Future[Boolean] =
    Audit.remove("volume_access", SQLTerms('volume -> volume.id, 'party -> party)).execute

  /** Determine what permission level the party has over the volume.
    * This takes into account full permission semantics including inheritance.
    * It purposefully takes a volume id which may not yet have been checked for permissions. */
  def check(volume : Volume.Id, party : Party.Id) : Future[Permission.Value] =
    SQL("SELECT volume_access_check(?, ?)")
      .apply(volume, party).single(SQLCols[Permission.Value])
}

