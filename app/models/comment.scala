package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import dbrary.SQL._
import site._

/** A comment made by a particular user applied to exactly one object.
  * These are immutable (and unaudited), although the author may be considered to have ownership. */
final class Comment private (val id : Comment.Id, val who : Account, val context : ContextSlot, val segment : Segment, val time : Timestamp, val text : String, val parents : Seq[Comment.Id])
  extends Slot with TableRowId[Comment] with InVolume {
  def whoId = who.id

  override def json = JsonRecord.flatten(id,
    Some('who -> who.party.json),
    Some('time -> time),
    Some('text -> text),
    if (parents.nonEmpty) Some('parents -> parents) else None
  ) ++ slotJson
}

object Comment extends TableId[Comment]("comment") with TableSlot[Comment] {
  private val columns = Columns(
      SelectColumn[Id]("id")
    , segment
    , SelectColumn[Timestamp]("time")
    , SelectColumn[String]("text")
    , SelectColumn[IndexedSeq[Id]]("thread")
    ).map { (id, segment, time, text, thread) =>
      new Comment(id, _ : Account, _ : ContextSlot, segment, time, text, thread.tail)
    } from "comment_thread AS comment"

  private def row(who : Selector[Account], container : Selector[Release.Value => Container]) =
    columns.join(
      who on "comment.who = account.id",
      ContextSlot.rowContainer(
        container on "comment.container = container.id",
        "comment.segment")
    ).map { case (comment, who, context) =>
      comment(who, context)
    }
  private val order = " ORDER BY comment.thread"

  /** Retrieve the set of all comments within the given volume. */
  private[models] def getVolume(volume : Volume) : Future[Seq[Comment]] =
    row(Account.row, Container.columnsVolume(Volume.fixed(volume)))
    .SELECT(PreparedStatement(order))
    .list

  /** Retrieve the set of all comments that apply to the given target. */
  private[models] def getSlot(slot : Slot) : Future[Seq[Comment]] =
    columns.join(
      Account.row on "comment.who = account.id",
      ContextSlot.rowContainer(slot.container, "comment.container", "comment.segment")
    ).map { case (comment, who, context) =>
      comment(who, context)
    }
    .SELECT(sql"WHERE comment.segment && ${slot.segment}::segment" + order)
    .list

  /** Retrieve the set of comments written by the specified user.
    * This checks permissions on the commented object (volume). */
  private[models] def getParty(who : Account)(implicit site : Site) : Future[Seq[Comment]] =
    row(Account.fixed(who), Container.columnsVolume(Volume.row))
    .SELECT(sql"WHERE " + Volume.condition + order)
    .list

  /** Post a new comment on a target by the current user.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  private[models] def post(slot : Slot, text : String, parent : Option[Id] = None) : Future[Comment] = {
    val site = slot.site.asInstanceOf[AuthSite]
    INSERT(slot.slotSql ++ SQLTerms('who -> site.identity.id, 'text -> text, 'parent -> parent), "id, time")
    .single(SQL.Cols[Id, Timestamp].map { (id, time) =>
      new Comment(id, site.account, slot.context, slot.segment, time, text, parent.toSeq)
    })
  }
}
