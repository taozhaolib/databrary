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
final class Container protected (override val id : Container.Id, override val volume : Volume, override val top : Boolean = false, private[this] var name_ : Option[String], private[this] var date_ : Option[Date]) extends ContextSlot with TableRowId[Container] with InVolume {
  override def container = this
  val segment = Segment.full
  override def isFull = true
  private[models] var consent_ = Consent.NONE
  override def consent = consent_
  /** Descriptive name to help with organization by contributors.
    * This (as with Container in general) is not necessarily intended for public consumption. */
  def name = name_
  /** The date at which the contained data were collected.
    * Note that this is covered (in part) by dataAccess permissions due to birthday/age restrictions. */
  def date = date_

  def ===(a : Container) = super[TableRowId].===(a)

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[Option[String]] = None, date : Option[Option[Date]] = None) : Future[Boolean] = {
    Audit.change("container", SQLTerms.flatten(name.map('name -> _), date.map('date -> _)), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        name.foreach(name_ = _)
        date.foreach(date_ = _)
      }
  }

  def remove : Future[Boolean] =
    (if (top) volume.top else async(null)).flatMap { stop =>
      if (top && ===(stop))
	async(false)
      else Audit.remove("container", sqlKey).execute
	.recover {
	  case e : com.github.mauricio.async.db.postgresql.exceptions.GenericDatabaseException if e.errorMessage.message.startsWith("update or delete on table \"container\" violates foreign key constraint ") => false
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
      consent.foreach(container.consent_ = _)
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

  /** Retrieve all the containers in a given volume. */
  private[models] def getVolume(v : Volume) : Future[Seq[Container]] =
    rowVolume(v).SELECT("ORDER BY container.top DESC")
    .apply().list

  /** Retrieve the top container in a given volume. */
  private[models] def getTop(v : Volume) : Future[Container] =
    rowVolume(v).SELECT("WHERE container.top ORDER BY container.id LIMIT 1")
    .apply().single

  /** Find the containers in a given volume with the given name. */
  def findName(v : Volume, name : String) : Future[Seq[Container]] =
    rowVolume(v).SELECT("WHERE container.name = ?")
    .apply(name).list

  /** Create a new container in the specified volume. */
  def create(volume : Volume, top : Boolean = false, name : Option[String] = None, date : Option[Date] = None)(implicit site : Site) : Future[Container] =
    Audit.add(table, SQLTerms('volume -> volume.id, 'top -> top, 'name -> name, 'date -> date), "id")
    .single(SQLCols[Id].map(new Container(_, volume, top, name, date)))
}
