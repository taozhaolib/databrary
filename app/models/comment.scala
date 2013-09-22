package models

import java.sql.{Timestamp,Date}
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import PGSegment.{column => segmentColumn,statement => segmentStatement}
import util._

/** A comment made by a particular user, usually only applied to exactly one object.
  * These are immutable (and unaudited), although the author may be considered to have ownership. */
final class Comment private (val id : Comment.Id, val who : Account, val when : Timestamp, val volume : Volume, val slot : Option[Slot], val text : String) extends TableRowId[Comment] with InVolume {
  def slotId = slot.map(_.id)
  def whoId = who.id
}

object Comment extends TableId[Comment]("comment") {
  private def make(who : Account, volume : Volume, slot : Option[Slot])(id : Comment.Id, when : Timestamp, text : String) =
    new Comment(id, who, when, volume, slot, text)
  private val columns = Columns[
    Id,  Timestamp, String](
    'id, 'when,     'text)
  private[models] val row = columns.
    join(Account.row, "comment.who = party.id").
    join(Volume.row, "comment.volume = volume.id").
    leftJoin(Slot.volumeColumns(), "comment.slot = slot.id") map {
      case (comment ~ who ~ volume ~ slot) => (make(who, volume, slot.map {
        case (slot ~ cont) => (Slot.make((Container.make(volume) _).tupled(cont)) _).tupled(slot)
      }) _).tupled(comment)
    }
  private def whoRow(who : Account) = columns.
    join(Volume.row, "comment.volume = volume.id").
    leftJoin(Slot.volumeColumns(), "comment.slot = slot.id") map {
      case (comment ~ volume ~ slot) => (make(who, volume, slot.map {
        case (slot ~ cont) => (Slot.make((Container.make(volume) _).tupled(cont)) _).tupled(slot)
      }) _).tupled(comment)
    }
  private def volumeRow(volume : Volume) = columns.
    join(Account.row, "comment.who = party.id").
    leftJoin(Slot.volumeRow(volume), "comment.slot = slot.id") map {
      case (comment ~ who ~ slot) => (make(who, volume, slot) _).tupled(comment)
    }
  private def volumeOnlyRow(volume : Volume) = columns.
    join(Account.row, "comment.who = party.id") map {
      case (comment ~ who) => (make(who, volume, None) _).tupled(comment)
    }
  private def containerRow(container : Container) = columns.
    join(Account.row, "comment.who = party.id").
    join(Slot.containerRow(container), "comment.slot = slot.id") map {
      case (comment ~ who ~ slot) => (make(who, container.volume, Some(slot)) _).tupled(comment)
    }
  private def slotRow(slot : Slot) = columns.
    join(Account.row, "comment.who = party.id") map {
      case (comment ~ who) => (make(who, slot.volume, Some(slot)) _).tupled(comment)
    }
  private val order = "ORDER BY comment.when DESC"

  /** Retrieve a specific comment by id.
    * This checks permissions on the commented object (volume). */
  def get(id : Id)(implicit site : Site) : Option[Comment] =
    row.SQL("WHERE comment.id = {id}").
      on('id -> id, 'identity -> site.identity.id).singleOpt

  /** Retrieve the set of comments within the given volume.
    * @param all include all indirect comments on any object within the given volume (which may be a lot) */
  private[models] def getVolume(volume : Volume, all : Boolean = true)(implicit db : Site.DB) : Seq[Comment] =
    (if (all) volumeRow(volume) else volumeOnlyRow(volume)).
      SQL("WHERE comment.volume = {volume}", (if (all) "" else "AND comment.slot IS NULL"), order).
      on('volume -> volume.id).list
  /** Retrieve the set of comments on the given target.
    * @param all include all indirect annotations on any containers, objects, or clips contained within the given target (which may be a lot) */
  private[models] def getSlot(slot : Slot, all : Boolean = true)(implicit db : Site.DB) : Seq[Comment] =
    if (all)
      containerRow(slot.container).SQL("WHERE slot.source = {cont} AND slot.segment <@ {seg}", order).
        on('cont -> slot.containerId, 'seg -> slot.segment).list
    else
      slotRow(slot).SQL("WHERE comment.slot = {slot}", order).
        on('slot -> slot.id).list
  /** Retrieve the set of comments written by the specified user.
    * This checks permissions on the commented object (volume). */
  private[models] def getParty(who : Account)(implicit site : Site) : Seq[Comment] =
    whoRow(who).SQL("WHERE who = {who}", order).
      on('who -> who.id, 'identity -> site.identity.id).list

  /** Post a new comment on a target by the current user.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  private[models] def post(target : Either[Volume,Slot], text : String)(implicit request : controllers.UserRequest[_]) : Comment = {
    val who = request.account
    val vol = target.merge
    val slot = target.right.toOption
    val args = SQLArgs('who -> who.id, 'volume -> vol.volumeId, 'slot -> slot.map(_.id), 'text -> text)
    SQL("INSERT INTO " + table + " " + args.insert + " RETURNING " + columns.select).
      on(args : _*).single(columns.map(make(who, vol.volume, slot) _))
  }
}

/** Objects on which comments may be placed. */
trait Commented extends InVolume {
  /** The list of comments on this object.
    * @param all include indirect comments on any contained objects
    */
  def comments(all : Boolean = true)(implicit db : Site.DB) : Seq[Comment]
  /** Post a new comment this object.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  def postComment(text : String)(implicit request : controllers.UserRequest[_]) : Comment
}
