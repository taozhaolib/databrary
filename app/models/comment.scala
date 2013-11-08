package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import site._

/** A comment made by a particular user applied to exactly one object.
  * These are immutable (and unaudited), although the author may be considered to have ownership. */
final class Comment private (val id : Comment.Id, val who : Account, val time : Timestamp, val slot : Slot, val text : String) extends TableRowId[Comment] with InVolume {
  def volume = slot.volume
  def slotId = slot.id
  def whoId = who.id
}

object Comment extends TableId[Comment]("comment") {
  private def make(who : Account, slot : Slot)(id : Comment.Id, time : Timestamp, text : String) =
    new Comment(id, who, time, slot, text)
  private val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Timestamp]("time")
    , SelectColumn[String]("text")
    )
  private def whoRow(who : Account)(implicit site : Site) = columns.
    join(Slot.row, "comment.slot = slot.id") map {
      case (comment, slot) => (make(who, slot) _).tupled(comment)
    }
  private def volumeRow(volume : Volume) = columns.
    join(Account.row(volume.site), "comment.who = party.id").
    join(Slot.volumeRow(volume), "comment.slot = slot.id") map {
      case ((comment, who), slot) => (make(who, slot) _).tupled(comment)
    }
  private def containerRow(container : Container) = columns.
    join(Account.row(container.site), "comment.who = party.id").
    join(Slot.containerRow(container), "comment.slot = slot.id") map {
      case ((comment, who), slot) => (make(who, slot) _).tupled(comment)
    }
  private def slotRow(slot : Slot) = columns.
    join(Account.row(slot.site), "comment.who = party.id") map {
      case (comment, who) => (make(who, slot) _).tupled(comment)
    }
  private val order = "ORDER BY comment.time DESC"

  /** Retrieve the set of all comments within the given volume. */
  private[models] def getVolume(volume : Volume) : Future[Seq[Comment]] =
    volumeRow(volume).SELECT("WHERE container.volume = ?", order).apply(volume.id).list

  /** Retrieve the set of comments on the given target.
    * @param all include all indirect annotations on any containers, objects, or clips contained within the given target */
  private[models] def getSlot(slot : Slot, all : Boolean = true) : Future[Seq[Comment]] =
    if (all)
      containerRow(slot.container).SELECT("WHERE slot.source = ? AND slot.segment <@ ?", order)
        .apply(slot.containerId, slot.segment).list
    else
      slotRow(slot).SELECT("WHERE comment.slot = ?", order).apply(slot.id).list

  /** Retrieve the set of comments written by the specified user.
    * This checks permissions on the commented object (volume). */
  private[models] def getParty(who : Account)(implicit site : Site) : Future[Seq[Comment]] =
    whoRow(who).SELECT("WHERE who = ? AND", Volume.condition, order).
      apply(who.id +: Volume.conditionArgs).list

  /** Post a new comment on a target by the current user.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  private[models] def post(slot : Slot, text : String)(implicit site : AuthSite) : Future[Comment] = {
    val who = site.identity
    val args = SQLTerms('who -> who.id, 'slot -> slot.id, 'text -> text)
    SQL("INSERT INTO " + table + " " + args.insert + " RETURNING " + columns.select)
      .apply(args).single(columns.parse.map(make(who, slot) _))
  }
}
