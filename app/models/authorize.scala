package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

/** A specific authorization by one party of another.
  * An authorization represents granting of certain permissions to a party by/under another, which can also be viewed as group membership.
  * Unlike most [TableRow] classes, these may be constructed directly, and thus are not expected to reflect the current state of the database in the same way and have different update semantics.
  * @constructor create an authorization object, not (yet) persisted to the database
  * @param childId the party being authorized, the "member"
  * @param parentId the party whose permissions are being authorized, the "group"
  * @param access the level of site access granted via the parent to the child, thus the maximum site permission level inherited by the child from the parent
  * @param delegate the specific permissions granted on behalf of the parent to the child, such tha the child has rights to perform actions up to this permission as the parent (not inherited)
  * @param authorized the time at which this authorization takes/took effect, or never (not yet) if None
  * @param expires the time at which this authorization stops, or never if None
  */
final case class Authorize(childId : Party.Id, parentId : Party.Id, access : Permission.Value, delegate : Permission.Value, authorized : Option[Timestamp], expires : Option[Timestamp]) extends TableRow {
  /** Update or add this authorization in the database.
    * If an authorization for the child and parent already exist, it is changed to match this.
    * Otherwise, a new one is added.
    * This may invalidate child.access. */
  def set(implicit site : Site) : Unit = {
    val id = SQLArgs('child -> childId, 'parent -> parentId)
    val args = SQLArgs('access -> access, 'delegate -> delegate, 'authorized -> authorized, 'expires -> expires)
    if (Audit.change(Authorize.table, args, id).executeUpdate() == 0)
      Audit.add(Authorize.table, args ++ id).execute()
  }
  /** Remove this authorization from the database.
    * Only child and parent are relevant for this operation.
    * This may invalidate child.access. */
  def remove(implicit site : Site) : Unit =
    Authorize.delete(childId, parentId)

  /** Determine if this authorization is currently in effect.
    * @return true if authorized is set and in the past, and expires is unset or in the future */
  def valid = {
    val now = (new java.util.Date).getTime
    authorized.fold(false)(_.getTime < now) && expires.fold(true)(_.getTime > now)
  }

  private[Authorize] val _child = CachedVal[Party, Site](Party.get(childId)(_).get)
  /** The child party being authorized. Cached. */
  def child(implicit site : Site) : Party = _child
  private[Authorize] val _parent = CachedVal[Party, Site](Party.get(parentId)(_).get)
  /** The parent party granting the authorization. Cached. */
  def parent(implicit site : Site) : Party = _parent
}

object Authorize extends Table[Authorize]("authorize") {
  private[models] val row = Columns[
    Party.Id, Party.Id, Permission.Value, Permission.Value, Option[Timestamp], Option[Timestamp]](
    'child,   'parent,  'access,          'delegate,        'authorized,       'expires).
    map(Authorize.apply _)

  private[this] val condition = "AND authorized < CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)"
  private[this] def conditionIf(all : Boolean) =
    if (all) "" else condition

  /** Retrieve a specific authorization identified by child and parent. */
  private[models] def get(child : Party.Id, parent : Party.Id)(implicit db : Site.DB) : Option[Authorize] =
    row.SQL("WHERE child = {child} AND parent = {parent}").
      on('child -> child, 'parent -> parent).singleOpt()

  /** Get all authorizations granted to a particular child.
    * @param all include inactive authorizations
    */
  private[models] def getParents(child : Party, all : Boolean = false)(implicit db : Site.DB) : Seq[Authorize] =
    row.join(Party.row, "parent = party.id").map { case (a ~ p) =>
        a._child() = child
        a._parent() = p
        a
      }.SQL("WHERE child = {child}", conditionIf(all)).
      on('child -> child.id).list
  /** Get all authorizations granted ba a particular parent.
    * @param all include inactive authorizations
    */
  private[models] def getChildren(parent : Party, all : Boolean = false)(implicit db : Site.DB) : Seq[Authorize] =
    row.join(Party.row, "child = party.id").map { case (a ~ c) =>
        a._child() = c
        a._parent() = parent
        a
      }.SQL("WHERE parent = {parent}", conditionIf(all)).
      on('parent -> parent.id).list

  /** Remove a particular authorization from the database.
    * @return true if a matching authorization was found and deleted
    */
  def delete(child : Party.Id, parent : Party.Id)(implicit site : Site) : Boolean =
    Audit.remove("authorize", SQLArgs('child -> child, 'parent -> parent)).
      execute()

  /** Determine the site access granted to a particular party.
    * This is defined by the minimum access level along a path of valid authorizations from [Party.Root], maximized over all possible paths, or Permission.NONE if there are no such paths. */
  private[models] def access_check(c : Party.Id)(implicit db : Site.DB) : Permission.Value =
    SQL("SELECT authorize_access_check({id})").
      on('id -> c).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)

  /** Determine the permission level granted to a child by a parent.
    * The child is granted all the same rights of the parent up to this level. */
  private[models] def delegate_check(child : Party.Id, parent : Party.Id)(implicit db : Site.DB) : Permission.Value =
    if (child == parent) Permission.ADMIN else // optimization
    SQL("SELECT authorize_delegate_check({child}, {parent})").
      on('child -> child, 'parent -> parent).single(scalar[Option[Permission.Value]]).
      getOrElse(Permission.NONE)
}
