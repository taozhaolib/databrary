package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** Collection of related assets.
  * To be used, all assets must be placed into containers.
  * These containers can represent a package of raw data acquired cotemporaneously or within a short time period (a single session), or a group of related materials.
  */
final class Container protected (override val id : Container.Id, override val volume : Volume, override val top : Boolean = false, val name_ : Option[String], val date_ : Option[Date]) extends ContextSlot with TableRowId[Container] with InVolume {
  override def container = this
  val segment = Segment.full
  override def isFull = true
  private[models] var _consent = Consent.NONE
  override def consent = _consent
  private[this] var _name = name_
  /** Descriptive name to help with organization by contributors.
    * This (as with Container in general) is not necessarily intended for public consumption. */
  def name = _name
  private[this] var _date = date_
  /** The date at which the contained data were collected.
    * Note that this is covered (in part) by dataAccess permissions due to birthday/age restrictions. */
  def date = _date

  def ===(a : Container) = super[TableRowId].===(a)

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[Option[String]] = None, date : Option[Option[Date]] = None) : Future[Boolean] = {
    Audit.change("container", SQLTerms.flatten(name.map('name -> _), date.map('date -> _)), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        name.foreach(_name = _)
        date.foreach(_date = _)
      }
  }

  override lazy val json : JsonRecord = JsonRecord.flatten(id,
    Some('volume -> volumeId),
    if (top) Some('top -> top) else None,
    getDate.map('date -> _.toString),
    Maybe(consent).opt.map('consent -> _),
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
      (volume : Volume) => new Container(id, volume, top, name, date)
    }
  private[models] def columnsVolume(volume : Selector[Volume]) = columns
    .join(volume, "container.volume = volume.id")
    .map(tupleApply)

  private val consent = Columns(
      SelectColumn[Consent.Value]("container_consent", "consent")
    ) from "slot_consent AS container_consent"
  private[models] def rowVolume(volume : Selector[Volume]) : Selector[Container] = columnsVolume(volume)
    .leftJoin(consent, "container.id = container_consent.container AND container_consent.segment = '(,)'::segment")
    .map { case (container, consent) =>
      consent.foreach(container._consent = _)
      container
    }
  private[models] def rowVolume(volume : Volume) : Selector[Container] =
    rowVolume(Volume.fixed(volume))
  private[models] def row(implicit site : Site) =
    rowVolume(Volume.row)

  /** Retrieve an individual Container.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Container]] =
    row.SELECT("WHERE container.id = ? AND", Volume.condition)
    .apply(i).singleOpt

  /** Retrieve all the (non-top) containers in a given volume. */
  private[models] def getVolume(v : Volume) : Future[Seq[Container]] =
    rowVolume(v).SELECT("WHERE NOT top ORDER BY date NULLS FIRST")
    .apply().list

  /** Retrieve the top container in a given volume. */
  private[models] def getTop(v : Volume) : Future[Container] =
    rowVolume(v).SELECT("WHERE top")
    .apply().single

  /** Find the containers in a given volume with the given name. */
  def findName(v : Volume, name : String) : Future[Seq[Container]] =
    rowVolume(v).SELECT("WHERE container.name = ?")
    .apply(name).list

  /** Create a new container in the specified volume. */
  def create(volume : Volume, name : Option[String] = None, date : Option[Date] = None)(implicit site : Site) : Future[Container] =
    Audit.add(table, SQLTerms('volume -> volume.id, 'name -> name, 'date -> date), "id")
    .single(SQLCols[Id].map(new Container(_, volume, false, name, date)))
}
