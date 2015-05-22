package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import dbrary.SQL._
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
  * @param individual the level of permission granted directly to the party.
  * @param children the level of permission granted to all descendents/members of the party
  */
final class VolumeAccess(val volume : Volume, val party : Party, val individual : Permission.Value, val children : Permission.Value) extends TableRow with InVolume {
  private[models] def sqlKey = SQLTerms('volume -> volumeId, 'party -> partyId)

  def partyId = party.id

  def json = JsonObject.flatten(
    Some('volume -> volume.json),
    Some('party -> party.json),
    Maybe(individual).opt('individual -> _),
    Maybe(children).opt('children -> _)
  )
}

object VolumeAccess extends Table[VolumeAccess]("volume_access") {
  private val columns = Columns(
      SelectColumn[Permission.Value]("individual")
    , SelectColumn[Permission.Value]("children")
    )
  private def row(volume : Selector[Volume], party : Selector[Party]) = columns.join(
      volume on "volume_access.volume = volume.id",
      party on "volume_access.party = party.id"
    ).map { case ((individual, children), volume, party) =>
      new VolumeAccess(volume, party, individual, children)
    }

  /** Retrieve the access entries for a volume. */
  private[models] def getParties(volume : Volume, access : Permission.Value = Permission.NONE) : Future[Seq[VolumeAccess]] =
    row(Volume.fixed(volume), Party.row)
    .SELECT(sql"WHERE individual >= $access ORDER BY individual DESC")
    .list
  /** Retrieve the volume access entries directly granted to a party for at least (READ). */
  private[models] def getVolumes(party : Party, access : Permission.Value = Permission.READ)(implicit site : Site) : Future[Seq[VolumeAccess]] =
    row(Volume.row, Party.fixed(party))
    .SELECT(sql"WHERE individual >= $access AND " + Volume.condition + " ORDER BY individual DESC")
    .list

  /** Update or add volume access in the database.
    * If an access for the volume and party already exists, it is changed to match this.
    * Otherwise, a new one is added.
    * If access is NONE, it is removed.
    * This may invalidate volume.access. */
  def set(volume : Volume, party : Party.Id, individual : Permission.Value = Permission.NONE, children : Permission.Value = Permission.NONE)(implicit site : Site) : Future[Boolean] =
    if (individual == Permission.NONE && children == Permission.NONE)
      Audit.remove("volume_access", SQLTerms('volume -> volume.id, 'party -> party)).execute
    else
      Audit.changeOrAdd("volume_access", SQLTerms('individual -> individual, 'children -> children), SQLTerms('volume -> volume.id, 'party -> party)).execute

  /** Determine what permission level the party has over the volume.
    * This takes into account full permission semantics including inheritance.
    * It purposefully takes a volume id which may not yet have been checked for permissions. */
  def check(volume : Volume.Id, party : Party.Id) : Future[Permission.Value] =
    sql"SELECT volume_access_check($volume, $party)"
      .run.single(SQL.Cols[Permission.Value])
}

