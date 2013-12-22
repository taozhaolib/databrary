package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

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
final class Authorize protected (val child : Party, val parent : Party, val access : Permission.Value, val delegate : Permission.Value, val authorized : Option[Timestamp], val expires : Option[Timestamp]) extends TableRow {
  def childId = child.id
  def parentId = parent.id

  /** Determine if this authorization is currently in effect.
    * @return true if authorized is set and in the past, and expires is unset or in the future */
  def valid = {
    authorized.fold(false)(_.toDateTime.isBeforeNow) && expires.fold(true)(_.toDateTime.isAfterNow)
  }
}

object Authorize extends Table[Authorize]("authorize") {
  private val columns = Columns(
      SelectColumn[Permission.Value]("access")
    , SelectColumn[Permission.Value]("delegate")
    , SelectColumn[Option[Timestamp]]("authorized")
    , SelectColumn[Option[Timestamp]]("expires")
    ).map { (access, delegate, authorized, expires) =>
      (child : Party, parent : Party) => new Authorize(child, parent, access, delegate, authorized, expires)
    }

  private[this] val condition = "AND authorized < CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)"
  private[this] def conditionIf(all : Boolean) =
    if (all) "" else condition

  /** Get all authorizations granted to a particular child.
    * @param all include inactive authorizations
    */
  private[models] def getParents(child : Party, all : Boolean = false) : Future[Seq[Authorize]] =
    columns.join(Party.row, "parent = party.id")
      .map { case (a, p) => a(child, p) }
      .SELECT("WHERE child = ?", conditionIf(all)).apply(child.id).list
  /** Get all authorizations granted ba a particular parent.
    * @param all include inactive authorizations
    */
  private[models] def getChildren(parent : Party, all : Boolean = false) : Future[Seq[Authorize]] =
    columns.join(Party.row, "child = party.id")
      .map { case (a, c) => a(c, parent) }
      .SELECT("WHERE parent = ?", conditionIf(all)).apply(parent.id).list

  /** Update or add a specific authorization in the database.
    * If an authorization for the child and parent already exist, it is changed to match this.
    * Otherwise, a new one is added.
    * This may invalidate child.access. */
  def set(child : Party.Id, parent : Party.Id, access : Permission.Value, delegate : Permission.Value = Permission.NONE, authorized : Option[Timestamp] = Some(new Timestamp), expires : Option[Timestamp] = None)(implicit site : Site) : Future[Boolean] =
    Audit.changeOrAdd(Authorize.table, SQLTerms('access -> access, 'delegate -> delegate, 'authorized -> authorized, 'expires -> expires), SQLTerms('child -> child, 'parent -> parent)).execute
  /** Remove a particular authorization from the database.
    * @return true if a matching authorization was found and deleted
    */
  def delete(child : Party.Id, parent : Party.Id)(implicit site : Site) : Future[Boolean] =
    Audit.remove("authorize", SQLTerms('child -> child, 'parent -> parent)).execute

  /** Determine the site access granted to a particular party.
    * This is defined by the minimum access level along a path of valid authorizations from [Party.Root], maximized over all possible paths, or Permission.NONE if there are no such paths. */
  private[models] def access_check(c : Party.Id) : Future[Permission.Value] = c match {
    case Party.NOBODY => Async(Permission.NONE) // anonymous users get this level
    case Party.ROOT => throw new IllegalArgumentException("trying to get root access") // the objective value is ADMIN but this should never be used
    case _ => SQL("SELECT authorize_access_check(?)").apply(c).single(SQLCols[Permission.Value])
  }

  /** Determine the permission level granted to a child by a parent.
    * The child is granted all the same rights of the parent up to this level. */
  private[models] def delegate_check(child : Party.Id, parent : Party.Id) : Future[Permission.Value] =
    if (child === parent) Async(Permission.ADMIN) else // optimization
    SQL("SELECT authorize_delegate_check(?, ?)")
      .apply(child, parent).single(SQLCols[Permission.Value])
}
