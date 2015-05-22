package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import macros._
import macros.async._
import dbrary._
import dbrary.SQL._
import site._

sealed trait AbstractTag extends Any {
  def name : String
  private[models] def get : Future[Tag]
  def add(slot : Slot, keyword : Boolean = false) : Future[Option[Tag]] =
    TagUse.add(this, slot, keyword)
  def remove(slot : Slot, keyword : Boolean = false) : Future[Option[Tag]] =
    TagUse.remove(this, slot, keyword)
  def set(slot : Slot, vote : Boolean, keyword : Boolean = false) : Future[Option[Tag]] =
    if (vote) add(slot, keyword) else remove(slot, keyword)
}

final class TagName private (val name : String) extends AnyVal with AbstractTag {
  override def toString = name
  private[models] def get = Tag.getOrCreate(this)
}

object TagName {
  private val validPattern = """\p{Lower}[-\p{Lower} ]{1,30}\p{Lower}""".r.pattern
  private val validRegex = """ *(\p{Alpha}[-\p{Alpha} ]{1,30}\p{Alpha}) *""".r

  def validate(name : String) : Option[TagName] = name match
    { case validRegex(tag) => Some(new TagName(tag.toLowerCase))
      case _ => None
    }

  /** Determine if the given tag name is valid.
    * @return the normalized name if valid or throw IllegalArgumentException */
  def apply(name : String) : TagName =
    validate(name).getOrElse(throw new IllegalArgumentException("invalid tag name"))

  implicit val sqlType : SQL.Type[TagName] = SQL.Type.transform[String,TagName]("varchar(32)", classOf[TagName])(n => Some(new TagName(n)), _.name)
}

/** All tags and their names used anywhere.
  * Immutable (create only).
  */
final class Tag private (val id : Tag.Id, val name : String) extends TableRowId[Tag] with AbstractTag {
  private[models] def get = async(this)

  def weight(slot : Slot) : Future[TagWeight] =
    TagWeight.getSlot(this, slot)
  def containers(implicit site : Site) : Future[Seq[TagWeightContainer]] =
    TagWeightContainer.get(this)
  def coverage(implicit site : Site) : Future[Seq[TagCoverage]] =
    TagCoverage.getTag(this)
  def coverage(slot : Slot) : Future[TagCoverage] =
    TagCoverage.getSlot(this, slot)

  def json : JsonRecord = JsonRecord(name)
  def json(options : JsonOptions.Options)(implicit site : Site) : Future[JsonRecord] =
    JsonOptions(json, options
    , "containers" -> (opt => containers.map(JsonArray.map(_.json)))
    , "coverage" -> (opt => coverage.map(JsonArray.map(c => c.slot.slotJson ++ c.json)))
    )
}

object Tag extends TableId[Tag]("tag") {
  private[models] def colId(t : AbstractTag) =
    Cols[Id].map(new Tag(_, t.name))

  private[models] val row = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    ).map { (id, name) =>
      new Tag(id, name)
    }

  /** Retrieve an individual tag by id. */
  def get(id : Id) : Future[Option[Tag]] =
    row.SELECT(sql"WHERE id = $id").singleOpt

  def _get(name : TagName) : Future[Option[Tag]] =
    row.SELECT(sql"WHERE name = $name").singleOpt

  /** Retrieve an individual tag by name. */
  def get(name : String) : Future[Option[Tag]] =
    TagName.validate(name).flatMapAsync(_get)

  /** Search for all tags names starting with the given string. */
  def search(name : String) : Future[Seq[Tag]] =
    TagName.validate(name).fold[Future[Seq[Tag]]](async(Nil)) { name =>
      row.SELECT(sql"WHERE name LIKE ${name+"%"}").list
    }

  /** Retrieve or, if none exists, create an individual tag by name. */
  private[models] def getOrCreate(name : TagName) : Future[Tag] =
    sql"SELECT get_tag($name)"
    .run.single(colId(name))
}

private[models] object TagUse extends Table[Unit]("tag_use") {
  private def table(keyword : Boolean = false) = if (keyword) "keyword_use" else "tag_use"

  private[models] def add(tag : AbstractTag, slot : Slot, keyword : Boolean = false) : Future[Option[Tag]] =
    (lsql"INSERT INTO " + table(keyword) ++ lsql" (tag, container, segment, who) VALUES ("
      ++ (tag match {
        case n : TagName => lsql"get_tag($n)"
        case t : Tag => lsql"${t.id}"
      }) ++ lsql", ${slot.containerId}, ${slot.segment}, ${slot.site.identity.id}) RETURNING tag")
    .run.singleOpt(Tag.colId(tag)).recover {
      case SQLDuplicateKeyException() => None
    }

  private[models] def remove(tag : AbstractTag, slot : Slot, keyword : Boolean = false) : Future[Option[Tag]] =
    (lsql"DELETE FROM ONLY " + table(keyword)
      ++ (tag match {
        case n : TagName => lsql" USING tag WHERE tag = tag.id AND name = $n"
        case t : Tag => lsql" WHERE tag = ${t.id}"
      }) ++ lsql" AND container = ${slot.containerId} AND segment <@ ${slot.segment}"
      ++ (if (keyword) "" else lsql" AND who = ${slot.site.identity.id}") + " RETURNING tag")
    .run.list(Tag.colId(tag)).map(_.headOption)
}

private[models] object KeywordUse extends Table[Unit]("keyword_use") {
  private[models] def add(tag : Tag, slot : Slot) = TagUse.add(tag, slot, true)
  private[models] def add(name : TagName, slot : Slot) = TagUse.add(name, slot, true)
  private[models] def remove(tag : Tag, slot : Slot) = TagUse.remove(tag, slot, true)
  private[models] def remove(name : TagName, slot : Slot) = TagUse.remove(name, slot, true)
}

sealed class TagWeight protected (val tag : Tag, val weight : Int, val keyword : Boolean, val vote : Boolean) {
  def json = JsonObject.flatten(
    Some('weight -> weight),
    if (keyword) Some('keyword -> keyword) else None,
    if (vote) Some('vote -> vote) else None
  )
}

/** Summary representation of tag information for a single tag and current user. */
final class TagWeightContainer private (tag : Tag, val container : Container, weight : Int, keyword : Boolean, vote : Boolean)
  extends TagWeight(tag, weight, keyword, vote) {
  override def json = container.json ++ super.json
}

private[models] sealed abstract class TagWeightView[T <: TagWeight] extends Table[T]("tag_weight") {
  protected def aggregateColumns(implicit site : Site) = Columns(
      SelectAs[Int]("count(tag_use.*)::integer", "weight")
    , SelectAs[Boolean]("COALESCE(bool_or(tag_use.tableoid = " + KeywordUse.tableOID + "), false)", "keyword")
    , SelectAs[Boolean](("COALESCE(bool_or(tag_use.tableoid = " + TagUse.tableOID) +: sql" AND tag_use.who = ${site.identity.id}), false)", "vote")
    )(FromTable("tag_use"))

  protected val groupColumn : SelectColumn[_]
  private[this] def groupBy = groupColumn.fromTable(FromTable("tag_use"))
  protected def columns(query : Statement)(implicit site : Site) =
    aggregateColumns.fromTable
    .fromQuery((aggregateColumns ~+ groupBy).statement + " " ++ query ++ (" GROUP BY " +: groupBy))
}

object TagWeight extends TagWeightView[TagWeight] {
  private def row(tag : Tag)(implicit site : Site) =
    aggregateColumns
    .map { (weight, keyword, up) =>
      new TagWeight(tag, weight, keyword, up)
    }

  protected val groupColumn = SelectColumn[Tag.Id]("tag")
  private def rows(query : Statement = EmptyStatement, limit : Int = 64)(implicit site : Site) =
    columns(query)
    .join(Tag.row on "tag_weight.tag = tag.id")
    .map { case ((weight, keyword, up), tag) =>
      new TagWeight(tag, weight, keyword, up)
    }
    .SELECT(sql"ORDER BY keyword DESC, weight DESC LIMIT $limit")
    .list

  private[models] def getSlot(tag : Tag, slot : Slot) =
    row(tag)(slot.site)
    .SELECT(sql"WHERE tag_use.tag = ${tag.id} AND tag_use.container = ${slot.containerId} AND tag_use.segment && ${slot.segment}::segment")
    .single

  /** Summarize all tags that overlap the given slot. */
  def getSlot(slot : Slot, limit : Int = 64) =
    rows(sql"WHERE tag_use.container = ${slot.containerId} AND tag_use.segment && ${slot.segment}::segment", limit)(slot.site)

  private[models] def getVolume(volume : Volume, limit : Int = 32) =
    rows(sql"JOIN container ON tag_use.container = container.id WHERE container.volume = ${volume.id}", limit)(volume.site)

  def getAll(limit : Int = 16)(implicit site : Site) =
    rows(limit = limit)
}

object TagWeightContainer extends TagWeightView[TagWeightContainer] {
  protected val groupColumn = SelectColumn[Container.Id]("container")
  private def rows(tag : Tag, query : Statement = EmptyStatement)(implicit site : Site) =
    columns(query)
    .join(Container.row on "tag_weight.container = container.id")
    .map { case ((weight, keyword, up), container) =>
      new TagWeightContainer(tag, container, weight, keyword, up)
    }
    .SELECT(sql"WHERE " + Volume.condition + " ORDER BY keyword DESC, weight DESC")
    .list

  private[models] def get(tag : Tag)(implicit site : Site) =
    rows(tag, sql"WHERE tag_use.tag = ${tag.id}")
}

final class TagCoverage private (val slot : Slot, tag : Tag, weight : Int, val coverage : Seq[Segment], val keywords : Seq[Segment], val votes : Seq[Segment])
  extends TagWeight(tag, weight, keywords.nonEmpty, votes.nonEmpty) {
  override def json = JsonObject.flatten(
    Some('weight -> weight),
    Some('coverage -> coverage),
    if (keywords.nonEmpty) Some('keyword -> keywords) else None,
    if (votes.nonEmpty) Some('vote -> votes) else None
  )
}

object TagCoverage extends Table[TagCoverage]("tag_coverage") {
  private def aggregateColumns(implicit site : Site) = Columns(
      SelectAs[Int]("count(tag_use.*)::integer", "weight")
    , SelectAs[IndexedSeq[Segment]]("segments_union(tag_use.segment)", "coverage")
    , SelectAs[IndexedSeq[Segment]]("segments_union(CASE WHEN tag_use.tableoid = " + KeywordUse.tableOID + " THEN tag_use.segment ELSE 'empty' END)", "keywords")
    , SelectAs[IndexedSeq[Segment]](("segments_union(CASE WHEN tag_use.tableoid = " + TagUse.tableOID) +: sql" AND tag_use.who = ${site.identity.id} THEN tag_use.segment ELSE 'empty' END)", "votes")
    )(FromTable("tag_use"))

  private[models] def getSlot(tag : Tag, slot : Slot) =
    aggregateColumns(slot.site)
    .map { (weight, coverage, keywords, votes) =>
      new TagCoverage(slot, tag, weight, coverage, keywords, votes)
    }
    .SELECT(sql"WHERE tag_use.tag = ${tag.id} AND tag_use.container = ${slot.containerId} AND tag_use.segment && ${slot.segment}::segment")
    .single

  private[models] def getSlot(slot : Slot, limit : Int = 64) = {
    implicit val site = slot.site
    val groupBy = SelectColumn[Tag.Id]("tag_use", "tag")
    aggregateColumns.fromTable
    .fromQuery((aggregateColumns ~+ groupBy).statement ++ sql" WHERE tag_use.container = ${slot.containerId} AND tag_use.segment && ${slot.segment}::segment GROUP BY " ++ groupBy)
    .join(Tag.row on "tag_coverage.tag = tag.id")
    .map { case ((weight, coverage, keywords, votes), tag) =>
      new TagCoverage(slot, tag, weight, coverage, keywords, votes)
    }
    .SELECT(sql"ORDER BY weight DESC LIMIT $limit")
    .list
  }

  private[models] def getTag(tag : Tag, limit : Int = 64)(implicit site : Site) = {
    val groupBy = SelectColumn[Tag.Id]("tag_use", "container")
    aggregateColumns.fromTable
    .fromQuery((aggregateColumns ~+ groupBy).statement ++ sql" WHERE tag_use.tag = ${tag.id} GROUP BY " ++ groupBy)
    .join(Container.row on "tag_coverage.container = container.id")
    .map { case ((weight, coverage, keywords, votes), container) =>
      new TagCoverage(container, tag, weight, coverage, keywords, votes)
    }
    .SELECT(sql"ORDER BY weight DESC LIMIT $limit")
    .list
  }
}
