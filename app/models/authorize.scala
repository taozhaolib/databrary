package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import dbrary.SQL._
import site._

/** Link between Authorize (rows in table/view) and Access. */
sealed class Authorization (val child : Party, val parent : Party, val site : Permission.Value, val member : Permission.Value) extends Access with TableRow {
  final def childId = child.id
  final def parentId = parent.id
  private[models] def sqlKey = SQLTerms('child -> childId, 'parent -> parentId)

  final def target = parent
  final def identity = child
}

/** A specific authorization by one party of another.
  * An authorization represents granting of certain permissions to a party by/under another, which can also be viewed as group membership.
  * Unlike most [TableRow] classes, these may be constructed directly, and thus are not expected to reflect the current state of the database in the same way and have different update semantics.
  * @constructor create an authorization object, not (yet) persisted to the database
  * @param child the party being authorized, the "member"
  * @param parent the party whose permissions are being authorized, the "group"
  * @param site the level of site/group access granted via the parent to the child, thus the maximum site permission level inherited by the child from the parent
  * @param member the specific permissions granted on behalf of the parent to the child, such that the child has rights to perform actions up to this permission as the parent (not inherited)
  * @param expires the time at which this authorization stops, or never if None
  */
final class Authorize protected (child : Party, parent : Party, site : Permission.Value, member : Permission.Value, val expires : Option[Timestamp]) extends Authorization(child, parent, site, member) {
  /** Determine if this authorization is currently in effect.
    * @return true if expires is unset or in the future */
  def valid = expires.forall(_.toDateTime.isAfterNow)
  /** True if this authorization has been "granted." */
  def authorized = site != Permission.NONE || member != Permission.NONE

  def json = JsonObject.flatten(
    Some('site -> site),
    Some('member -> member),
    expires.map('expires -> _)
  )
}

object Authorize extends Table[Authorize]("authorize") {
  private val columns = Columns(
      SelectColumn[Permission.Value]("site")
    , SelectColumn[Permission.Value]("member")
    , SelectColumn[Option[Timestamp]]("expires")
    )
    .map { (site, member, expires) =>
      (child : Party, parent : Party) => new Authorize(child, parent, site, member, expires)
    }

  private[this] val condition = " AND (site > 'NONE' OR member > 'NONE') AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)"
  private[this] def conditionIf(all : Boolean) =
    if (all) "" else condition

  def get(child : Party, parent : Party) : Future[Option[Authorize]] =
    columns
      .map(_(child, parent))
      .SELECT(sql"WHERE child = ${child.id} AND parent = ${parent.id}")
      .singleOpt

  /** Get all authorizations granted to a particular child.
    * @param all include inactive authorizations
    */
  private[models] def getParents(child : Party, all : Boolean = false) : Future[Seq[Authorize]] =
    columns.join(Party.row on "parent = party.id")
    .map { case (a, p) => a(child, p) }
    .SELECT(sql"WHERE child = ${child.id}" + conditionIf(all))
    .list
  /** Get all authorizations granted ba a particular parent.
    * @param all include inactive authorizations
    */
  private[models] def getChildren(parent : Party, all : Boolean = false) : Future[Seq[Authorize]] =
    columns.join(Party.row on "child = party.id")
    .map { case (a, c) => a(c, parent) }
    .SELECT(sql"WHERE parent = ${parent.id}" + conditionIf(all))
    .list

  def getAll() : Future[Seq[Authorize]] =
    columns.join(
      Party.columns.fromAlias("child") on "child = child.id",
      Party.columns.fromAlias("parent") on "parent = parent.id"
    ).map { case (auth, child, parent) => auth(child, parent) }
    .SELECT(sql"ORDER BY parent.id, site")
    .list

  def apply(child : Party.Id, parent : Party.Id)(implicit request : Site) : Future[Boolean] =
    Audit.add(Authorize.table, SQLTerms('child -> child, 'parent -> parent)).execute
  /** Update or add a specific authorization in the database.
    * If an authorization for the child and parent already exist, it is changed to match this.
    * Otherwise, a new one is added.
    * This may invalidate child.access. */
  def set(child : Party.Id, parent : Party.Id, site : Permission.Value, member : Permission.Value, expires : Option[Timestamp] = None)(implicit request : Site) : Future[Boolean] =
    Audit.changeOrAdd(Authorize.table, SQLTerms('site -> site, 'member -> member, 'expires -> expires), SQLTerms('child -> child, 'parent -> parent)).execute
  /** Remove a particular authorization from the database.
    * @return true if a matching authorization was found and deleted
    */
  def remove(child : Party.Id, parent : Party.Id)(implicit site : Site) : Future[Boolean] =
    Audit.remove("authorize", SQLTerms('child -> child, 'parent -> parent)).execute
}

object Authorization extends Table[Authorization]("authorize_view") {
  private val columns = Columns(
      SelectColumn[Permission.Value]("site")
    , SelectColumn[Permission.Value]("member")
    )

  private def unOpt(access : Option[(Permission.Value, Permission.Value)]) : (Permission.Value, Permission.Value) =
    access.getOrElse((Permission.NONE, Permission.NONE))

  object Nobody extends Authorization(Party.Nobody, Party.Root, Permission.NONE, Permission.NONE)
  object Root extends Authorization(Party.Root, Party.Root, Permission.ADMIN, Permission.ADMIN)

  private final class Self (party : Party) extends Authorization(party, party,
    if (party === Party.Nobody) Permission.NONE else Permission.ADMIN,
    if (party === Party.Nobody) Permission.NONE else Permission.ADMIN)

  private def make(child : Party, parent : Party = Party.Root)(access : Option[(Permission.Value, Permission.Value)]) : Authorization = {
    val (site, member) = unOpt(access)
    new Authorization(child, parent, site, member)
  }

  private[models] def rowParent(parent : Party = Party.Root) =
    Party.row.join(
      columns on_? sql"party.id = authorize_view.child AND authorize_view.parent = ${parent.id}"
    ).map { case (p, a) => make(p, parent)(a) }
  private[models] def rowChild(child : Party) =
    Party.row.join(
      columns on_? sql"party.id = authorize_view.parent AND authorize_view.child = ${child.id}"
    ).map { case (p, a) => make(child, p)(a) }

  /** Determine the effective inherited and direct permission levels granted to a child by a parent. */
  private[models] def get(child : Party, parent : Party = Party.Root) : Future[Authorization] =
    if (child === parent) async(new Self(parent)) // optimization
    else columns
      .SELECT(sql"WHERE child = ${child.id} AND parent = ${parent.id}")
      .singleOpt.map(make(child, parent))
}
