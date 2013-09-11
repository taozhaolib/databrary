package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

/** Access control over a volume granted to a party.
  * Volume access permissions (ACLs) are determined by a set of VolumeAccess entries (ACEs).
  * Note that volume access can also be granted to descendents ("members") of the assigned party in one of two ways: through explicit inheritance within VolumeAccess, or through delegated permissions within an Authorization.
  * Only one of these is necessary to gain access (they are unioned, not intersected).
  *
  * Unlike most [TableRow] classes, these may be constructed directly, and thus are not expected to reflect the current state of the database in the same way and have different update semantics.
  * @constructor create an access object, not (yet) persisted to the database
  * @param volumeId the volume to which access is being granted; the target
  * @param partyId the party being granted the access; the user
  * @param access the level of permission granted directly to the party. Levels at or above [[Permission.EDIT]] are considered volume "membership."
  * @param inherit the level of permission granted to all descendents/members of the party, which cannot be [[Permission.ADMIN]]
  */
final case class VolumeAccess(volumeId : Volume.Id, partyId : Party.Id, access : Permission.Value, inherit : Permission.Value) extends TableRow {
  /** Update or add this access in the database.
    * If an access for the volume and party already exist, it is changed to match this.
    * Otherwise, a new one is added.
    * This may invalidate volume.access. */
  def set(implicit site : Site) : Unit = {
    val id = SQLArgs('volume -> volumeId, 'party -> partyId)
    val args =  SQLArgs('access -> access, 'inherit -> inherit)
    if (Audit.change("volume_access", args, id).executeUpdate() == 0)
      Audit.add("volume_access", args ++ id).execute()
  }
  /** Remove this access from the database.
    * Only volume and party are relevant for this operation.
    * This may invalidate volume.access. */
  def remove(implicit site : Site) : Unit =
    VolumeAccess.delete(volumeId, partyId)

  private val _volume = CachedVal[Volume, Site](Volume.get(volumeId)(_).get)
  /** The volume to which access is being granted.
    * If the current user does not have access to volume, this may throw an exception. */
  def volume(implicit site : Site) : Volume = _volume
  private val _party = CachedVal[Party, Site](Party.get(partyId)(_).get)
  /** The party being granted access. */
  def party(implicit site : Site) : Party = _party
}

object VolumeAccess extends Table[VolumeAccess]("volume_access") {
  private[models] val row = Columns[
    Volume.Id, Party.Id, Permission.Value, Permission.Value](
    'volume,   'party,   'access,          'inherit).
    map(VolumeAccess.apply _)

  /** Retrieve a specific volume access identified by volume and party.
    * This does not check for appropriate permissions on volume, so is unsafe. */
  private[models] def get(volume : Volume.Id, party : Party.Id)(implicit db : Site.DB) : Option[VolumeAccess] =
    SELECT("WHERE volume = {volume} AND party = {party}").
      on('volume -> volume, 'party -> party).singleOpt()

  /** Retrieve the access entries for a volume at or above the specified permission level. */
  private[models] def getParties(volume : Volume, permission : Permission.Value = Permission.NONE)(implicit db : Site.DB) =
    JOIN(Party, "ON (party = id) WHERE volume = {volume} AND access >= {access} ORDER BY access DESC").
      on('volume -> volume.id, 'access -> permission).list((row ~ Party.row).
        map({ case (a ~ e) => a._party() = e; a._volume() = volume; a })
      )
  /** Retrieve the volume access entries granted to a party at or above the specified permission level. */ 
  private[models] def getVolumes(party : Party, permission : Permission.Value = Permission.NONE)(implicit site : Site) =
    JOIN(Volume, "ON (volume = id) WHERE party = {party} AND access >= {access} AND " + Volume.condition + " ORDER BY access DESC").
      on('party -> party.id, 'access -> permission, 'identity -> site.identity.id).list((row ~ Volume.row).
        map({ case (a ~ s) => a._volume() = s; a._party() = party; a })
      )

  /** Remove a particular volume access from the database.
    * @return true if a matching volume access was found and deleted
    */
  def delete(volume : Volume.Id, party : Party.Id)(implicit site : Site) =
    Audit.remove("volume_access", SQLArgs('volume -> volume, 'party -> party)).
      execute()

  /** Determine what permission level the party has over the volume.
    * This takes into account full permission semantics including inheritance.
    * It purposefully takes a volume id which may not yet have been checked for permissions. */
  def check(volume : Volume.Id, party : Party.Id)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT volume_access_check({volume}, {party})").
      on('volume -> volume, 'party -> party).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)
}

