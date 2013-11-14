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
final class Container protected (val id : Container.Id, val volume : Volume, val top : Boolean = false, val name_ : Option[String], val date_ : Option[Date]) extends TableRowId[Container] with InVolume {
  def container = this
  private[this] var _name = name_
  /** Descriptive name to help with organization by contributors.
    * This (as with Container in general) is not necessarily intended for public consumption. */
  def name = _name
  private[this] var _date = date_
  /** The date at which the contained data were collected.
    * Note that this is covered (in part) by dataAccess permissions due to birthday/age restrictions. */
  def date = _date

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[String] = _name, date : Option[Date] = _date) : Future[Boolean] = {
    if (name == _name && date == _date)
      return Async(true)
    Audit.change("container", SQLTerms('name -> name, 'date -> date), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        _name = name
        _date = date
      }
  }

  /** List of contained assets within this container.
    * In most cases calling `fullSlot.assets` makes more sense. */
  def assets : Future[Seq[ContainerAsset]] = ContainerAsset.getContainer(this)

  /** List of slots on this container. */
  def slots : Future[Seq[Slot]] = Slot.getContainer(this)
  private[models] var _fullSlot : Slot = null /* Should always be set on construction. */
  /** Slot that covers this entire container and which thus serves as a proxy for display and metadata. */
  def fullSlot : Slot = _fullSlot
}

object Container extends TableId[Container]("container") {
  private val base = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Boolean]("top")
    , SelectColumn[Option[String]]("name")
    , SelectColumn[Option[Date]]("date")
    ).map { (id, top, name, date) =>
      (vol : Volume) => new Container(id, vol, top, name, date)
    }
  private val full = base
    .join(Slot.columns(true).fromAlias("full_slot"), "full_slot.source = container.id AND full_slot.segment = '(,)'")
    .map { case (cont, full) =>
      (vol : Volume) =>
        val c = cont(vol)
        c._fullSlot = full(c)
        c
    }
  private def columns(haveFull : Boolean) =
    if (haveFull) base else full
  private[models] def row(full : Boolean)(implicit site : Site) =
    Volume.row.join(columns(full), "container.volume = volume.id") map {
      case (vol, cont) => cont(vol)
    }
  private[models] def volumeRow(volume : Volume, full : Boolean = false) =
    columns(full).map(_(volume))

  /** Retrieve an individual Container.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Container]] =
    row(false).SELECT("WHERE container.id = ? AND", Volume.condition)
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
    for {
      cont <- Audit.add(table, SQLTerms('volume -> volume.id, 'name -> name, 'date -> date), "id")
        .single(SQLCols[Id].map(new Container(_, volume, false, name, date)))
      full <- Slot.containerRow(cont, true)
        .SELECT("WHERE slot.source = ? AND slot.segment = '(,)'")
        .apply(cont.id).single
    } yield {
      cont._fullSlot = full
      cont
    }
}
