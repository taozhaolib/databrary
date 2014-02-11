package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** Link between Authorize (rows in table/view) and Access. */
sealed class Authorization (val child : Party, val parent : Party, val inherit : Permission.Value, val direct : Permission.Value) extends Access with TableRow {
  final def childId = child.id
  final def parentId = parent.id
  private[models] def sqlKey = SQLTerms('child -> childId, 'parent -> parentId)

  final def target = parent
  final def identity = child
  final val group = inherit
}

/** A specific authorization by one party of another.
  * An authorization represents granting of certain permissions to a party by/under another, which can also be viewed as group membership.
  * Unlike most [TableRow] classes, these may be constructed directly, and thus are not expected to reflect the current state of the database in the same way and have different update semantics.
  * @constructor create an authorization object, not (yet) persisted to the database
  * @param childId the party being authorized, the "member"
  * @param parentId the party whose permissions are being authorized, the "group"
  * @param inherit the level of site/group access granted via the parent to the child, thus the maximum site permission level inherited by the child from the parent
  * @param delegate the specific permissions granted on behalf of the parent to the child, such that the child has rights to perform actions up to this permission as the parent (not inherited)
  * @param authorized the time at which this authorization takes/took effect, or never (not yet) if None
  * @param expires the time at which this authorization stops, or never if None
  */
final class Authorize protected (child : Party, parent : Party, inherit : Permission.Value, direct : Permission.Value, val authorized : Option[Timestamp], val expires : Option[Timestamp]) extends Authorization(child, parent, inherit, direct) {
  /** Determine if this authorization is currently in effect.
    * @return true if authorized is set and in the past, and expires is unset or in the future */
  def valid =
    authorized.exists(_.toDateTime.isBeforeNow) && expires.forall(_.toDateTime.isAfterNow)

  def json = JsonObject.flatten(
    Some('inherit -> inherit),
    Some('direct -> direct),
    authorized.map('authorized -> _),
    expires.map('expires -> _)
  )
}

object Authorize extends Table[Authorize]("authorize") {
  private val columns = Columns(
      SelectColumn[Permission.Value]("inherit")
    , SelectColumn[Permission.Value]("direct")
    , SelectColumn[Option[Timestamp]]("authorized")
    , SelectColumn[Option[Timestamp]]("expires")
    ).map { (inherit, direct, authorized, expires) =>
      (child : Party, parent : Party) => new Authorize(child, parent, inherit, direct, authorized, expires)
    }

  private[this] val condition = "AND authorized <= CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)"
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
  def set(child : Party.Id, parent : Party.Id, inherit : Permission.Value, direct : Permission.Value, authorized : Option[Timestamp] = Some(new Timestamp), expires : Option[Timestamp] = None)(implicit site : Site) : Future[Boolean] =
    Audit.changeOrAdd(Authorize.table, SQLTerms('inherit -> inherit, 'direct -> direct, 'authorized -> authorized, 'expires -> expires), SQLTerms('child -> child, 'parent -> parent)).execute
  /** Remove a particular authorization from the database.
    * @return true if a matching authorization was found and deleted
    */
  def delete(child : Party.Id, parent : Party.Id)(implicit site : Site) : Future[Boolean] =
    Audit.remove("authorize", SQLTerms('child -> child, 'parent -> parent)).execute
}

object Authorization extends Table[Authorization]("authorize_volume") {
  private[models] val columns = Columns(
      SelectColumn[Permission.Value]("inherit")
    , SelectColumn[Permission.Value]("direct")
    )

  private def unOpt(access : Option[(Permission.Value, Permission.Value)]) : (Permission.Value, Permission.Value) =
    access.getOrElse((Permission.NONE, Permission.NONE))

  object Nobody extends Authorization(Party.Nobody, Party.Root, Permission.NONE, Permission.NONE)

  private final class Self (party : Party) extends Authorization(party, party,
    if (party.id === Party.NOBODY) Permission.NONE else Permission.ADMIN,
    if (party.id === Party.NOBODY) Permission.NONE else Permission.ADMIN)

  private[models] def make(child : Party, parent : Party = Party.Root)(access : Option[(Permission.Value, Permission.Value)]) : Authorization = {
    val (inherit, direct) = unOpt(access)
    new Authorization(child, parent, inherit, direct)
  }

  /** Determine the effective inherited and direct permission levels granted to a child by a parent. */
  private[models] def get(child : Party, parent : Party = Party.Root) : Future[Authorization] =
    if (child === parent) Async(new Self(child)) // optimization
    else columns
      .SELECT("WHERE child = ? AND parent = ?")
      .apply(child.id, parent.id).singleOpt
      .map(make(child, parent))
}
