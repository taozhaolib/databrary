package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import PGSegment.{column => segmentColumn,statement => segmentStatement}
import site._

/** All tags and their names used anywhere.
  * Immutable (create only).
  */
final class Tag private (val id : Tag.Id, val name : String) extends TableRowId[Tag] {
  /** Set the current user's tag value (up, down, or none) for the slot. */
  def set(slot : Slot, up : Option[Boolean] = Some(true))(implicit site : AuthSite) : Unit =
    up.fold(TagUse.remove(this, slot))(TagUse.set(this, slot, _))
}

object Tag extends TableId[Tag]("tag") {
  private def make(id : Id, name : String) =
    new Tag(id, name)
  private[models] val row = Columns[
    Id,  String](
    'id, 'name).
    map(make _)

  /** Retrieve an individual tag by id. */
  private[models] def get(id : Id)(implicit db : Site.DB) : Option[Tag] =
    row.SQL("WHERE id = {id}").on('id -> id).singleOpt

  /** Retrieve an individual tag by name. */
  private[models] def get(name : String)(implicit db : Site.DB) : Option[Tag] =
    row.SQL("WHERE name = {name}").on('name -> name).singleOpt

  private val validRegex = """ *\p{Alpha}[-\p{Alpha} ]{0,31} *""".r

  /** Determine if the given tag name is valid.
    * @return the normalized name if valid */
  private[models] def valid(name : String) : Option[String] = name match
    { case validRegex() => Some(name.trim.toLowerCase)
      case _ => None
    }

  /** Search for all tags containing the given string within their name. */
  def search(name : String)(implicit db : Site.DB) : Seq[Tag] =
    valid(name).fold[Seq[Tag]](Nil) { name =>
      row.SQL("WHERE name LIKE {name}").on('name -> ("%" + name + "%")).list
    }

  /** Retrieve or, if none exists, create an individual tag by name. */
  private[models] def getOrCreate(name : String)(implicit db : Site.DB) : Tag =
    DBUtil.selectOrInsert(get(name)) {
      val args = SQLArgs('name -> name)
      SQL("INSERT INTO tag " + args.insert + " RETURNING " + row.select).
        on(args : _*).single(row)
    }
}

/** A tag applied by a user to an object. */
final class TagUse private (val tag : Tag, val who : Account, val slot : Slot, val up : Boolean = true) extends TableRow with InVolume {
  def volume = slot.volume
  def slotId = slot.id
  def whoId = who.id
  def weight = if (up) 1 else -1

  def remove(implicit db : Site.DB) : Unit = {
    val ids = SQLArgs('tag -> tag.id, 'slot -> slot.id, 'who -> who.id)
    SQL("DELETE FROM tag_use WHERE " + ids).execute
  }
}

object TagUse extends Table[TagUse]("tag_use") {
  private def make(tag : Tag, who : Account, slot : Slot)(up : Boolean) =
    new TagUse(tag, who, slot, up)
  private[models] val columns = Columns[
    Boolean](
    'up)
  private[models] val aggregate =
    Columns[Boolean](SelectAs("bool_or(tag_use.up)", "agg_up"))
  private[models] val row = columns.
    join(Tag.row, "tag_use.tag = tag.id").
    join(Account.row, "comment.who = party.id").
    join(Slot.row, "comment.slot = slot.id") map {
      case (up ~ tag ~ who ~ slot) => make(tag, who, slot)(up)
    }

  private[models] def remove(tag : Tag, slot : Slot)(implicit site : Site) : Unit = {
    val ids = SQLArgs('tag -> tag.id, 'slot -> slot.id, 'who -> site.identity.id)
    SQL("DELETE FROM tag_use WHERE " + ids.where).on(ids:_*).execute
  }

  private[models] def set(tag : Tag, slot : Slot, up : Boolean = true)(implicit site : AuthSite) : TagUse = {
    val who = site.identity
    val ids = SQLArgs('tag -> tag.id, 'slot -> slot.id, 'who -> who.id)
    val args = ids ++ SQLArgs('up -> up)
    DBUtil.updateOrInsert(
      SQL("UPDATE tag_use SET up = {up} WHERE " + ids.where).on(args : _*))(
      SQL("INSERT INTO tag_use " + args.insert).on(args : _*))
    new TagUse(tag, who, slot, up)
  }
}

/** Summary representation of tag information for a single slot and current user. */
final case class TagWeight private (tag : Tag, weight : Int, user : Option[Boolean] = None) extends TableRow

object TagWeight extends Table[TagWeight]("tag_weight") {
  private val columns = Columns[
    Int](
    'weight)
  private val useJoinOn = "tag_weight.tag = tag_use.tag AND tag_use.slot = {mainslot} AND tag_use.who = {who}"
  private[models] val row = columns.
    join(Tag.row, "tag_weight.tag = tag.id").
    leftJoin(TagUse.columns, useJoinOn).map {
      case (weight ~ tag ~ up) => TagWeight(tag, weight, up)
    }
  private val aggregate =
    Columns[Int](SelectAs("sum(tag_weight.weight)::int", "agg_weight")).
    join(Tag.row, "tag_weight.tag = tag.id").
    leftJoin(TagUse.aggregate, useJoinOn).map {
      case (weight ~ tag ~ up) => TagWeight(tag, weight, up)
    }

  private[models] def getSlot(slot : Slot, all : Boolean = true)(implicit site : Site) : Seq[TagWeight] =
    if (all)
      aggregate.SQL("""
         JOIN slot ON tag_weight.slot = slot.id 
        WHERE slot.source = {cont} AND slot.segment <@ {seg}
        GROUP BY tag.id, tag.name
        HAVING sum(tag_weight.weight) > 0
        ORDER BY agg_weight DESC""").
        on('mainslot -> slot.id, 'cont -> slot.containerId, 'seg -> slot.segment, 'who -> site.identity.id).list
    else
      row.SQL("WHERE tag_weight.slot = {slot} ORDER BY weight DESC").
        on('mainslot -> slot.id, 'slot -> slot.id, 'who -> site.identity.id).list

  private[models] def getVolume(volume : Volume)(implicit site : Site) : Seq[TagWeight] =
    aggregate.SQL("""
       JOIN slot ON tag_weight.slot = slot.id 
       JOIN container ON slot.source = container.id 
      WHERE container.volume = {volume} 
      GROUP BY tag.id, tag.name
      HAVING sum(tag_weight.weight) > 0
      ORDER BY agg_weight DESC""").
      on('volume -> volume.id, 'mainslot -> volume.topSlot.id, 'who -> site.identity.id).list
}
