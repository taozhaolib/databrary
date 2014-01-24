package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import site._

/** A comment made by a particular user applied to exactly one object.
  * These are immutable (and unaudited), although the author may be considered to have ownership. */
final class Comment private (val id : Comment.Id, val who : Account, val time : Timestamp, val slot : Slot, val text : String, val parentId : Option[Comment.Id]) extends TableRowId[Comment] with InVolume {
  def volume = slot.volume
  def slotId = slot.id
  def whoId = who.id

  lazy val json = JsonRecord.flatten(id,
    Some('who -> who.party.json),
    Some('time -> time),
    Some('text -> text),
    parentId.map('parent -> _)
  ) ++ slot.json
}

object Comment extends TableId[Comment]("comment") {
  private val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Timestamp]("time")
    , SelectColumn[String]("text")
    , SelectColumn[Option[Id]]("parent")
    ).map { (id, time, text, parent) =>
      (who : Account, slot : Slot) => new Comment(id, who, time, slot, text, parent)
    }
  /* XXX use here of comment_thread is inefficient, as it always threads the whole comment table, even if we only need a subset. */
  private val threads = columns from "comment_thread AS comment";
  private def whoRow(who : Account)(implicit site : Site) = columns
    .join(Slot.row, "comment.slot = slot.id") map {
      case (comment, slot) => comment(who, slot)
    }
  private def volumeRow(volume : Volume) = threads
    .join(Account.row, "comment.who = party.id")
    .join(Slot.volumeRow(volume), "comment.slot = slot.id") map {
      case ((comment, who), slot) => comment(who, slot)
    }
  private def containerRow(container : Container) = threads
    .join(Account.row, "comment.who = party.id")
    .join(Slot.containerRow(container), "comment.slot = slot.id") map {
      case ((comment, who), slot) => comment(who, slot)
    }
  private def slotRow(slot : Slot) = threads
    .join(Account.row, "comment.who = party.id") map {
      case (comment, who) => comment(who, slot)
    }
  private val order = "ORDER BY comment.thread"

  /** Retrieve the set of all comments within the given volume. */
  private[models] def getVolume(volume : Volume) : Future[Seq[Comment]] =
    volumeRow(volume).SELECT("WHERE container.volume = ?", order).apply(volume.id).list

  /** Retrieve the set of comments on the given target. */
  private[models] def getSlot(slot : Slot) : Future[Seq[Comment]] =
    slotRow(slot).SELECT("WHERE comment.slot = ?", order).apply(slot.id).list

  /** Retrieve the set of all comments that apply to the given target. */
  private[models] def getSlotAll(slot : AbstractSlot) : Future[Seq[Comment]] =
    containerRow(slot.container).SELECT("WHERE slot.source = ? AND slot.segment <@ ?::segment", order)
      .apply(slot.containerId, slot.segment).list

  /** Retrieve the set of comments written by the specified user.
    * This checks permissions on the commented object (volume). */
  private[models] def getParty(who : Account)(implicit site : Site) : Future[Seq[Comment]] =
    whoRow(who).SELECT("WHERE who = ? AND", Volume.condition, "ORDER BY comment.id DESC").
      apply(who.id).list

  /** Post a new comment on a target by the current user.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  private[models] def post(slot : AbstractSlot, text : String, parent : Option[Id] = None)(implicit site : AuthSite) : Future[Boolean] =
    SQL("INSERT INTO comment (who, text, parent, slot) VALUES (?, ?, ?,", slot.sqlId, ")")
      .apply(SQLArgs(site.identity.id, text, parent) ++ slot.sqlArgs).execute
}
