package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import site._

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
final case class VolumeAccess(volume : Volume, partyId : Party.Id, access : Permission.Value, inherit : Permission.Value) extends TableRow with InVolume {
  /** Update or add this access in the database.
    * If an access for the volume and party already exist, it is changed to match this.
    * Otherwise, a new one is added.
    * This may invalidate volume.access. */
  def set(implicit site : Site) : Unit = {
    val id = SQLArgs('volume -> volumeId, 'party -> partyId)
    val args =  SQLArgs('access -> access, 'inherit -> inherit)
    DBUtil.updateOrInsert(Audit.change("volume_access", args, id))(
      Audit.add("volume_access", args ++ id))
  }
  /** Remove this access from the database.
    * Only volume and party are relevant for this operation.
    * This may invalidate volume.access. */
  def remove(implicit site : Site) : Unit =
    VolumeAccess.delete(volumeId, partyId)

  private val _party = CachedVal[Party, Site](Party.get(partyId)(_).get)
  /** The party being granted access. Cached. */
  def party(implicit site : Site) : Party = _party
}

object VolumeAccess extends Table[VolumeAccess]("volume_access") {
  private def make(volume : Volume)(partyId : Party.Id, access : Permission.Value, inherit : Permission.Value) =
    new VolumeAccess(volume, partyId, access, inherit)
  private val columns = Columns[
    Party.Id, Permission.Value, Permission.Value](
    'party,   'access,          'inherit)
  private[models] val row = columns.join(Volume.row, "volume_access.volume = volume.id") map
    { case (a ~ vol) => (make(vol) _).tupled(a) }
  private def volumeRow(volume : Volume) = columns map (make(volume) _)

  /** Retrieve a specific volume access identified by volume and party.
    * This checks permissions on volume. */
  private[models] def get(volume : Volume, party : Party.Id)(implicit db : Site.DB) : Option[VolumeAccess] =
    volumeRow(volume).SQL("WHERE volume = {volume} AND party = {party}").
      on('volume -> volume.id, 'party -> party).singleOpt()

  /** Retrieve the access entries for a volume. */
  private[models] def getParties(volume : Volume)(implicit db : Site.DB) : Seq[VolumeAccess] =
    volumeRow(volume).join(Party.row, "party = id").
      map { case (a ~ e) => a._party() = e; a }.
      SQL("WHERE volume = {volume} ORDER BY access DESC, party.name").
      on('volume -> volume.id).list
  /** Retrieve the volume access entries granted to a party at or above the specified permission level. */ 
  private[models] def getVolumes(party : Party, permission : Permission.Value = Permission.NONE)(implicit site : Site) : Seq[VolumeAccess] =
    row.map { a => a._party() = party; a }.
      SQL("WHERE party = {party} AND access >= {access} AND", Volume.condition, "ORDER BY access DESC, volume.body").
      on(Volume.conditionArgs('party -> party.id, 'access -> permission) : _*).list

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

