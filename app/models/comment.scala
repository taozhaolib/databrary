package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** A comment made by a particular user applied to exactly one object.
  * These are immutable (and unaudited), although the author may be considered to have ownership. */
final class Comment private (val id : Comment.Id, val who : Account, val segment : Segment, val context : ContextSlot, val time : Timestamp, val text : String, val parentId : Option[Comment.Id]) extends TableRowId[Comment] with Slot with InVolume {
  def whoId = who.id

  override lazy val json = JsonRecord.flatten(id,
    Some('who -> who.party.json),
    Some('time -> time),
    Some('text -> text),
    parentId.map('parent -> _)
  ) ++ slotJson
}

object Comment extends TableId[Comment]("comment") with TableSlot[Comment] {
  override protected type A = Account => Comment
  private val columns : Selector[ContextSlot => A] = Columns(
      SelectColumn[Id]("id")
    , segment
    , SelectColumn[Timestamp]("time")
    , SelectColumn[String]("text")
    , SelectColumn[Option[Id]]("parent")
    ).map { (id, segment, time, text, parent) =>
      (context : ContextSlot) => (who : Account) =>
	new Comment(id, who, segment, context, time, text, parent)
    }
  /* XXX use here of comment_thread is inefficient, as it always threads the whole comment table, even if we only need a subset. */
  private val threads = columns from "comment_thread AS comment";

  private def row(who : Selector[Account], container : Selector[Container]) =
    columnsSlot(threads, container, false)
    .join(who, "comment.who = account.id")
    .map(tupleApply)
  private def rowContainer(container : Selector[Container]) =
    row(Account.row, container)
  private val order = "ORDER BY comment.thread"

  /** Retrieve the set of all comments within the given volume. */
  private[models] def getVolume(volume : Volume) : Future[Seq[Comment]] =
    rowContainer(Container.columnsVolume(Volume.fixed(volume)))
    .SELECT(order)
    .apply().list

  /** Retrieve the set of all comments that apply to the given target. */
  private[models] def getSlot(slot : Slot) : Future[Seq[Comment]] =
    rowContainer(Container.fixed(slot.container))
    .SELECT("WHERE comment.segment && ?::segment", order)
    .apply(slot.segment).list

  /** Retrieve the set of comments written by the specified user.
    * This checks permissions on the commented object (volume). */
  private[models] def getParty(who : Account)(implicit site : Site) : Future[Seq[Comment]] =
    row(Account.fixed(who), Container.row)
    .SELECT("WHERE", Volume.condition, order)
    .apply().list

  /** Post a new comment on a target by the current user.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  private[models] def post(slot : Slot, text : String, parent : Option[Id] = None)(implicit site : AuthSite) : Future[Boolean] =
    INSERT(slot.slotSql ++ SQLTerms('who -> site.identity.id, 'text -> text, 'parent -> parent)).execute
}
