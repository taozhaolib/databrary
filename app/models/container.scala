package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import dbrary.SQL._
import site._

/** Collection of related assets.
  * To be used, all assets must be placed into containers.
  * These containers can represent a package of raw data acquired cotemporaneously or within a short time period (a single session), or a group of related materials.
  * @param name descriptive name to help with organization by contributors.
  * @param date the date at which the contained data were collected. Note that this is covered (in part) by dataAccess permissions due to birthday/age restrictions.
  */
final class Container protected (val id : Container.Id, override val volume : Volume, override val top : Boolean = false, val name : Option[String], val date : Option[Date], override val release : Release.Value)
  extends ContextSlot with TableRowId[Container] {
  override def container = this
  def segment = Segment.full

  def ===(a : Container) = super[TableRowId].===(a)

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[Option[String]] = None, date : Option[Option[Date]] = None) : Future[Container] =
    Audit.change("container", SQLTerms.flatten(name.map('name -> _), date.map('date -> _)), SQLTerms('id -> id)).ensure
      .map { _ =>
        new Container(id, volume, top, name.getOrElse(this.name), date.getOrElse(this.date), release)
      }

  def remove : Future[Boolean] =
    (if (top) volume.top else async(null)).flatMap { stop =>
      if (top && ===(stop))
        async(false)
      else Audit.remove("container", sqlKey).execute
        .recover {
          case SQLException(e) if e.startsWith("update or delete on table \"container\" violates foreign key constraint ") => false
        }
    }

  override def pageParent = Some(volume)

  override lazy val json : JsonRecord = JsonRecord.flatten(id,
    Some('volume -> volumeId),
    if (top) Some('top -> top) else None,
    getDate.map(d => 'date -> org.joda.time.format.ISODateTimeFormat.date.print(d).replaceAllLiterally("\ufffd", "X")),
    releaseJson,
    name.map('name -> _)
  )
}

object Container extends TableId[Container]("container") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Boolean]("top")
    , SelectColumn[Option[String]]("name")
    , SelectColumn[Option[Date]]("date")
    ).map { (id, top, name, date) =>
      (new Container(id, _ : Volume, top, name, date, _ : Release.Value)).curried
    }
  private[models] def columnsVolume(volume : Selector[Volume]) =
    columns
    .join(volume on "container.volume = volume.id")
    .map(tupleApply)

  private def rowVolume(volume : Selector[Volume]) : Selector[Container] =
    columnsVolume(volume)
    .join(SlotRelease.release on_? "container.id = slot_release.container AND slot_release.segment = '(,)'")
    .map { case (container, release) => container(release.getOrElse(Release.DEFAULT)) }
  private[models] def rowVolume(volume : Volume) : Selector[Container] =
    rowVolume(Volume.fixed(volume))
  private[models] def row(implicit site : Site) =
    rowVolume(Volume.row)

  /** Retrieve an individual Container.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Container]] =
    row.SELECT(sql"WHERE container.id = $i AND" + Volume.condition)
    .singleOpt

  /** Retrieve all the containers in a given volume. */
  private[models] def getVolume(v : Volume) : Future[Seq[Container]] =
    rowVolume(v).SELECT(sql"ORDER BY container.top DESC")
    .list

  /** Retrieve the top container in a given volume. */
  private[models] def getTop(v : Volume) : Future[Container] =
    rowVolume(v).SELECT(sql"WHERE container.top ORDER BY container.id LIMIT 1")
    .single

  /** Create a new container in the specified volume. */
  def create(volume : Volume, top : Boolean = false, name : Option[String] = None, date : Option[Date] = None)(implicit site : Site) : Future[Container] =
    Audit.add(table, SQLTerms('volume -> volume.id, 'top -> top, 'name -> name, 'date -> date), "id")
    .single(SQL.Cols[Id].map(new Container(_, volume, top, name, date, Release.DEFAULT)))
}
