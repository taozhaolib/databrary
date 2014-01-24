package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
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

  def json : JsonRecord = JsonRecord(name)
  def json(options : JsonOptions.Options)(implicit site : Site) : Future[JsonRecord] =
    JsonOptions(json, options,
      "slots" -> (opt => SlotWeight.getTag(this).map(JsonArray.map(_.json)))
    )
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

  private val validRegex = """ *\p{Alpha}[-\p{Alpha} ]{1,30}\p{Alpha} *""".r

  /** Determine if the given tag name is valid.
    * @return the normalized name if valid */
  private[models] def valid(name : String) : Option[String] = name match
    { case validRegex() => Some(name.trim.toLowerCase)
      case _ => None
    }

  /** Retrieve an individual tag by name. */
  def get(name : String) : Future[Option[Tag]] =
    valid(name).fold[Future[Option[Tag]]](Async(None))(_get(_))

  /** Search for all tags containing the given string within their name. */
  def search(name : String) : Future[Seq[Tag]] =
    valid(name).fold[Future[Seq[Tag]]](Async(Nil)) { name =>
      row.SELECT("WHERE name LIKE ?").apply("%" + name + "%").list
    }

  /** Retrieve or, if none exists, create an individual tag by name. */
  private[models] def getOrCreate(name : String) : Future[Tag] =
    SQL("SELECT get_tag(?)")
      .apply(name).single(SQLCols[Id].map(new Tag(_, name)))

  // private[models] def set(name : String, slot : Slot, up : Option[Boolean] = Some(true))(implicit site : AuthSite) : Future[Boolean] =
}

/** A tag applied by a user to an object.
  * This is (currently) unused. */
final class TagUse private (val tag : Tag, val who : Account, val segment : Segment, val context : ContextSlot, val up : Boolean = true) extends TableRow with Slot with InVolume {
  def whoId = who.id
  def weight = if (up) 1 else -1

  def remove() = TagUse.remove(tag, this, who)
}

object TagUse extends Table[TagUse]("tag_use") with TableSlot[TagUse] {
  private def remove(tag : Tag, slot : Slot, who : Account) : Future[Boolean] =
    DELETE('tag -> tag.id, 'container -> slot.containerId, 'segment -> slot.segment, 'who -> who.id).execute

  private[models] def remove(tag : Tag, slot : Slot)(implicit site : AuthSite) : Future[Boolean] =
    remove(tag, slot, site.account)

  private[models] def set(tag : Tag, slot : Slot, up : Boolean = true)(implicit site : AuthSite) : Future[Boolean] = {
    val who = site.identity
    val ids = SQLTerms('tag -> tag.id, 'container -> slot.containerId, 'segment -> slot.segment, 'who -> who.id)
    val args = ('up -> up) +: ids
    DBUtil.updateOrInsert(
      SQL("UPDATE tag_use SET up = ? WHERE", ids.where)(_, _).apply(args))(
      INSERT(args)(_, _)).execute
  }
}

private[models] abstract sealed class Weight protected (val weight : Int, val user : Option[Boolean] = None) extends TableRow {
  def json : JsonObject = JsonObject.flatten(
    Some('weight -> weight),
    user.map(u => 'vote -> (if (u) 1 else -1))
  )
}

/** Summary representation of tag information for a single slot and current user. */
final class TagWeight private (val tag : Tag, weight : Int, user : Option[Boolean] = None) extends Weight(weight, user) {
  override def json = tag.json ++ super.json
}

/** Summary representation of tag information for a single tag and current user. */
final class ContainerWeight private (val container : Container, weight : Int, user : Option[Boolean] = None) extends Weight(weight, user) {
  override def json = container.json ++ super.json
}

private[models] sealed class WeightView[T <: Weight] extends Table[T]("tag_use") {
  protected val base = Columns(
      SelectAs[Int]("SUM(CASE WHEN up THEN 1::integer ELSE -1::integer END)::integer", "weight")
    , SelectAs[Option[Boolean]]("bool_or(CASE WHEN who = ? THEN up ELSE NULL END)", "user")
    )
  protected def columns(implicit site : Site) = 
    base.pushArgs(SQLArgs(site.identity.id))
}

object TagWeight extends WeightView[TagWeight] {
  private def row(implicit site : Site) = columns
    .join(Tag.row, "tag_use.tag = tag.id").map {
      case ((weight, up), tag) => new TagWeight(tag, weight, up)
    }

  /** Summarize all tags that overlap the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[TagWeight]] =
    row(slot.site).SELECT("""
      WHERE tag_use.container = ? AND tag_use.segment && ?::segment
      GROUP BY tag.id, tag.name
      ORDER BY weight DESC""")
    .apply(slot.containerId, slot.segment).list

  private[models] def getVolume(volume : Volume) : Future[Seq[TagWeight]] =
    row(volume.site).SELECT("""
       JOIN container ON tag_use.container = container.id 
      WHERE container.volume = ? 
      GROUP BY tag.id, tag.name
      ORDER BY weight DESC""")
    .apply(volume.id).list
}

object ContainerWeight extends WeightView[SlotWeight] {
  private def row(implicit site : Site) = columns
    .join(Container.row, "tag_use.container = container.id").map {
      case ((weight, up), container) => new ContainerWeight(container, weight, up)
    }

  private[models] def getTag(tag : Tag)(implicit site : Site) : Future[Seq[SlotWeight]] =
    row
    .SELECT("WHERE tag_use.tag = ? AND", Volume.condition, "GROUP BY container.id ORDER BY weight DESC")
    .apply(tag.id).list
}
