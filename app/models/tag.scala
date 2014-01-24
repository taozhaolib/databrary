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
      "containers" -> (opt => ContainerWeight.getTag(this).map(JsonArray.map(_.json)))
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
  private[models] val aggregateColumns = Columns(
      SelectAs[Int]("SUM(CASE WHEN up THEN 1::integer ELSE -1::integer END)::integer", "weight")
    , SelectAs[Option[Boolean]]("bool_or(CASE WHEN who = ? THEN up ELSE NULL END)", "user")
    )

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
  override def json = container.containerJson ++ super.json
}

private[models] sealed abstract class WeightView[T <: Weight] extends Table[T]("tag_weight") {
  protected val groupColumn : SelectColumn[_]
  private[this] def groupBy = groupColumn.fromTable(FromTable("tag_use"))
  private[this] def aggregate =
    TagUse.aggregateColumns ~+ groupBy
  protected def columns(query : String*)(args : SQLArgs)(implicit site : Site) =
    TagUse.aggregateColumns.fromTable(table)
    .from(aggregate.SELECT(query ++ Seq("GROUP BY", groupBy.toString) : _*))
    .pushArgs(site.identity.id +: args)
}

object TagWeight extends WeightView[TagWeight] {
  protected val groupColumn = SelectColumn[Tag.Id]("tag")
  private def get(query : String*)(args : SQLArgs)(implicit site : Site) =
    columns(query : _*)(args)
    .join(Tag.row, "tag_weight.tag = tag.id").map {
      case ((weight, up), tag) => new TagWeight(tag, weight, up)
    }
    .SELECT("WHERE weight > 0 OR user IS NOT NULL ORDER BY weight DESC")
    .list

  /** Summarize all tags that overlap the given slot. */
  private[models] def getSlot(slot : Slot) : Future[Seq[TagWeight]] =
    get("WHERE tag_use.container = ? AND tag_use.segment && ?::segment")
      (SQLArgs(slot.containerId, slot.segment))

  private[models] def getVolume(volume : Volume) : Future[Seq[TagWeight]] =
    get("JOIN container ON tag_use.container = container.id WHERE container.volume = ?")
      (SQLArgs(volume.id))
}

object ContainerWeight extends WeightView[ContainerWeight] {
  protected val groupColumn = SelectColumn[Container.Id]("container")
  private def get(query : String*)(args : SQLArgs)(implicit site : Site) =
    columns(query : _*)(args)
    .join(Container.row, "tag_weight.container = container.id").map {
      case ((weight, up), container) => new ContainerWeight(container, weight, up)
    }
    .SELECT("WHERE (weight > 0 OR user IS NOT NULL) AND", Volume.condition, "ORDER BY weight DESC")
    .list

  private[models] def getTag(tag : Tag)(implicit site : Site) : Future[Seq[ContainerWeight]] =
    get("WHERE tag_use.tag = ?")(SQLArgs(tag.id))
}
