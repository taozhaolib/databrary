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
final class Container protected (override val id : Container.Id, override val volume : Volume, val top : Boolean = false, val name_ : Option[String], val date_ : Option[Date], consent_ : Consent.Value = Consent.NONE) extends Slot(id, Container.range, consent_) with TableRowId[Container] with InVolume {
  def container = this
  override def isFull = true
  override def isTop = top
  def context = this
  private[this] var _name = name_
  /** Descriptive name to help with organization by contributors.
    * This (as with Container in general) is not necessarily intended for public consumption. */
  def name = _name
  private[this] var _date = date_
  /** The date at which the contained data were collected.
    * Note that this is covered (in part) by dataAccess permissions due to birthday/age restrictions. */
  def date = _date

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[Option[String]] = None, date : Option[Option[Date]] = None) : Future[Boolean] = {
    Audit.change("container", SQLTerms.flatten(name.map('name -> _), date.map('date -> _)), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        name.foreach(_name = _)
        date.foreach(_date = _)
      }
  }

  /** List of slots on this container. */
  def slots : Future[Seq[Slot]] = Slot.getContainer(this)

  override lazy val json : JsonRecord = JsonRecord.flatten(id,
    Some('volume -> volumeId),
    if (top) Some('top -> top) else None,
    name.map('name -> _),
    getDate.map('date -> _.toString)
  )
}

object Container extends TableId[Container]("container") {
  final val range : Range[Offset] = Range.full[Offset]
  private val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Boolean]("top")
    , SelectColumn[Option[String]]("name")
    , SelectColumn[Option[Date]]("date")
    , SelectColumn[Consent.Value]("full_slot", "consent")
    ).map { (id, top, name, date, consent) =>
      (vol : Volume) => new Container(id, vol, top, name, date, consent)
    } from "container JOIN slot AS full_slot USING (id)"
  private[models] def row(implicit site : Site) =
    Volume.row.join(columns, "container.volume = volume.id") map {
      case (vol, cont) => cont(vol)
    }
  private[models] def volumeRow(volume : Volume) =
    columns.map(_(volume))

  /** Retrieve an individual Container.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Container]] =
    row.SELECT("WHERE container.id = ? AND", Volume.condition)
      .apply(i +: Volume.conditionArgs).singleOpt

  /** Retrieve all the (non-top) containers in a given volume. */
  private[models] def getVolume(v : Volume) : Future[Seq[Container]] =
    volumeRow(v).SELECT("WHERE container.volume = ? AND NOT top ORDER BY date, id")
      .apply(v.id).list

  /** Retrieve the top container in a given volume. */
  private[models] def getTop(v : Volume) : Future[Container] =
    volumeRow(v).SELECT("WHERE container.volume = ? AND top")
      .apply(v.id).single

  /** Find the containers in a given volume with the given name. */
  def findName(v : Volume, name : String) : Future[Seq[Container]] =
    volumeRow(v).SELECT("WHERE container.volume = ? AND container.name = ?")
      .apply(v.id, name).list

  /** Create a new container in the specified volume. */
  def create(volume : Volume, name : Option[String] = None, date : Option[Date] = None)(implicit site : Site) : Future[Container] =
    Audit.add(table, SQLTerms('volume -> volume.id, 'name -> name, 'date -> date), "id")
      .single(SQLCols[Id].map(new Container(_, volume, false, name, date)))
}
