package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import macros._
import dbrary._
import site._

/** All tags and their names used anywhere.
  * Immutable (create only).
  */
final class Tag private (val id : Tag.Id, val name : String) extends TableRowId[Tag] {
  /** Set the current user's tag value (up, down, or none) for the slot. */
  def set(slot : Slot, up : Option[Boolean] = Some(true))(implicit site : AuthSite) : Future[Boolean] =
    up.fold(TagUse.remove(this, slot))(TagUse.set(this, slot, _))
}

object Tag extends TableId[Tag]("tag") {
  private[models] val row = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    ).map { (id, name) =>
      new Tag(id, name)
    }

  /** Retrieve an individual tag by id. */
  private[models] def get(id : Id) : Future[Option[Tag]] =
    row.SELECT("WHERE id = ?").apply(id).singleOpt

  private def _get(name : String)(implicit dbc : Site.DB, exc : ExecutionContext) : Future[Option[Tag]] =
    row.SELECT("WHERE name = ?")(dbc, exc).apply(name).singleOpt

  /** Retrieve an individual tag by name. */
  private[models] def get(name : String) : Future[Option[Tag]] =
    _get(name)

  private val validRegex = """ *\p{Alpha}[-\p{Alpha} ]{1,30}\p{Alpha} *""".r

  /** Determine if the given tag name is valid.
    * @return the normalized name if valid */
  private[models] def valid(name : String) : Option[String] = name match
    { case validRegex() => Some(name.trim.toLowerCase)
      case _ => None
    }

  /** Search for all tags containing the given string within their name. */
  def search(name : String) : Future[Seq[Tag]] =
    valid(name).fold[Future[Seq[Tag]]](Async(Nil)) { name =>
      row.SELECT("WHERE name LIKE ?").apply("%" + name + "%").list
    }

  /** Retrieve or, if none exists, create an individual tag by name. */
  private[models] def getOrCreate(name : String) : Future[Tag] =
    DBUtil.selectOrInsert(_get(name)(_, _)) { (dbc, exc) =>
      val args = SQLTerms('name -> name)
      SQL("INSERT INTO tag", args.insert, "RETURNING", row.select)(dbc, exc)
        .apply(args).single(row.parse)
    }
}

/** A tag applied by a user to an object. */
final class TagUse private (val tag : Tag, val who : Account, val slot : Slot, val up : Boolean = true) extends TableRow with InVolume {
  def volume = slot.volume
  def slotId = slot.id
  def whoId = who.id
  def weight = if (up) 1 else -1

  def remove() = TagUse.remove(tag, slot, who)
}

object TagUse extends Table[TagUse]("tag_use") {
  private[models] val columns =
    Columns(SelectColumn[Boolean]("up"))
  private[models] val aggregate =
    Columns(SelectAs[Boolean]("bool_or(tag_use.up)", "agg_up"))

  private[models] def remove(tag : Tag, slot : Slot, who : Account) : Future[Boolean] =
    DELETE('tag -> tag.id, 'slot -> slot.id, 'who -> who.id).execute

  private[models] def remove(tag : Tag, slot : Slot)(implicit site : AuthSite) : Future[Boolean] =
    remove(tag, slot, site.account)

  private[models] def set(tag : Tag, slot : Slot, up : Boolean = true)(implicit site : AuthSite) : Future[Boolean] = {
    val who = site.identity
    val ids = SQLTerms('tag -> tag.id, 'slot -> slot.id, 'who -> who.id)
    val args = ('up -> up) +: ids
    DBUtil.updateOrInsert(
      SQL("UPDATE tag_use SET up = ? WHERE", ids.where)(_, _).apply(args))(
      INSERT(args)(_, _)).execute
  }
}

/** Summary representation of tag information for a single slot and current user. */
final case class TagWeight private (tag : Tag, weight : Int, user : Option[Boolean] = None) extends TableRow {
  lazy val json =
    JsonRecord.flatten(tag.name,
      Some('weight -> weight),
      user.map(u => 'vote -> (if (u) 1 else -1))
    )
}

object TagWeight extends Table[TagWeight]("tag_weight") {
  private val useJoinOn = "tag_weight.tag = tag_use.tag AND tag_use.slot = ? AND tag_use.who = ?"
  private val row = 
    Columns(SelectColumn[Int]("weight"))
    .join(Tag.row, "tag_weight.tag = tag.id")
    .leftJoin(TagUse.columns, useJoinOn).map {
      case ((weight, tag), up) => TagWeight(tag, weight, up)
    }
  private val aggregate =
    Columns(SelectAs[Int]("sum(tag_weight.weight)::int", "agg_weight"))
    .join(Tag.row, "tag_weight.tag = tag.id")
    .leftJoin(TagUse.aggregate, useJoinOn).map {
      case ((weight, tag), up) => TagWeight(tag, weight, up)
    }

  private[models] def getSlot(slot : Slot, all : Boolean = true)(implicit site : Site) : Future[Seq[TagWeight]] =
    if (all)
      aggregate.SELECT("""
         JOIN slot ON tag_weight.slot = slot.id 
        WHERE slot.source = ? AND slot.segment <@ ?::segment
        GROUP BY tag.id, tag.name
        HAVING sum(tag_weight.weight) > 0 OR count(tag_use.up) > 0
        ORDER BY agg_weight DESC""")
        .apply(slot.id, site.identity.id, slot.containerId, slot.segment).list
    else
      row.SELECT("WHERE tag_weight.slot = ? ORDER BY weight DESC")
        .apply(slot.id, site.identity.id, slot.id).list

  private[models] def getVolume(volume : Volume)(implicit site : Site) : Future[Seq[TagWeight]] =
    volume.top.flatMap { topSlot =>
      aggregate.SELECT("""
         JOIN slot ON tag_weight.slot = slot.id 
         JOIN container ON slot.source = container.id 
        WHERE container.volume = ? 
        GROUP BY tag.id, tag.name
        HAVING sum(tag_weight.weight) > 0 OR count(tag_use.up) > 0
        ORDER BY agg_weight DESC""")
        .apply(topSlot.id, site.identity.id, volume.id).list
    }
}
