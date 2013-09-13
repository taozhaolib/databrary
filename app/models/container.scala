package models

import anorm._
import anorm.SqlParser.scalar
import java.sql.Date
import dbrary._
import dbrary.Anorm._
import util._

/** Collection of related assets.
  * To be used, all assets must be place into collections.
  * These collections can represent a package of raw data acquired cotemporaneously or within a short time period (a single session), or a group of related materials.
  * Critically, all assets within a collection share the same annotations.
  */
final class Container protected (val id : Container.Id, val volume : Volume, val date_ : Option[Date]) extends TableRowId[Container] with SitePage {
  def volumeId = volume.id
  private[this] var _date = date_
  /** The date at which the contained data were collected.
    * Note that this is covered (in part) by dataAccess permissions due to birthday/age restrictions. */
  def date = _date

  /** Update the given values in the database and this object in-place. */
  def change(date : Option[Date] = _date)(implicit site : Site) : Unit = {
    if (date == _date)
      return
    Audit.change("container", SQLArgs('date -> date), SQLArgs('id -> id)).execute()
    _date = date
  }

  /** Permission granted to the current site user for this container, defined by the containing volume and determined at lookup time. */
  def permission : Permission.Value = volume.permission

  /** List of contained assets within this container. */
  private[this] val _assets = CachedVal[Seq[AssetLink], Site.DB](AssetLink.getAssets(this)(_))
  def assets(implicit db : Site.DB) : Seq[AssetLink] = _assets
  /** Look up a specific contained asset. */
  def getAsset(o : Asset.Id)(implicit db : Site.DB) = AssetLink.get(this, o)

  def pageName(implicit site : Site) = date.toString // FIXME date permissions/useful title
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL = controllers.routes.Container.view(id).url
}

/** Smallest organizatonal unit of related data.
  * Primarily used for an individual session of data with a single date and place.
  * Importantly contained data must be covered by a single consent level. */
final class Slot private (val id : Slot.Id, val container : Container, val segment : Range[Offset], val consent_ : Option[Consent.Value]) extends Container(id) with TableRowId[Slot] {
  def volumeId = volume.id
  private[this] var _consent = consent_
  def consent = _consent

  /** Update the given values in the database and this object in-place. */
  def change(consent : Option[Consent.Value] = _consent)(implicit site : Site) : Unit = {
    if (consent == _consent)
      return
    Audit.change("slot_consent", SQLArgs('consent -> maybe(consent, Consent.NONE)), SQLArgs('slot -> id)).execute()
    _consent = consent
  }

  def pageName(implicit site : Site) = date.toString
  def pageParent(implicit site : Site) = Some(volume)
  def pageURL = controllers.routes.Slot.view(id).url

  /** The level of access granted on data covered by this slot to the current user. */
  def dataAccess(classification : Classification.Value = Classification.RESTRICTED)(implicit site : Site) =
    Permission.data(permission, consent, classification)
}


/** Base for container interfaces. */
private[models] sealed abstract class ContainerView[R <: Container with TableRowId[R]](table : String) extends TableId[R](table) {
  protected final val permission = "volume_access_check(volume.id, {identity})"
  private[models] final val condition = permission + " >= 'VIEW'"

  def get(i : Id)(implicit site : Site) : Option[R]
}

object Container extends ContainerView[Container]("container") {
  private[models] val row =
    (Volume.row ~ Slot.columns.?) map {
      case (volume ~ None) => volume
      case (volume ~ Some(slot)) => Slot.baseMake(volume)(slot)
    }
  private[models] override val src = """container 
    LEFT JOIN slot USING (id) 
         JOIN volume ON volume.id = container.id OR volume.id = slot.volume"""

  /** Retrieve an individual Container, according to its type.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Container] =
    SELECT("WHERE container.id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve the set of containers to which the given annotation is attached.
    * @return viewable containers ordered by volume, date */
  def getAnnotation(annotation : Annotation)(implicit site : Site) : Seq[Container] =
    SELECT("JOIN container_annotation ON container.id = container WHERE annotation = {annotation} ORDER BY volume.id, slot.date, slot.id").
      on('annotation -> annotation.id, 'identity -> site.identity.id).list()
}

object Slot extends ContainerView[Slot]("slot") {
  private[models] def makeVolume(volume : Volume)(id : Id, consent : Option[Consent.Value], date : Date) = new Slot(id, volume, consent.getOrElse(Consent.NONE), date)
  private[models] def baseMake(volume : Volume) = (makeVolume(volume) _).tupled
  private[models] val columns = Columns[
    Id,  Option[Consent.Value], Date](
    'id, 'consent,              'date)
  private[models] val row = (Volume.row ~ columns) map {
    case (volume ~ slot) => baseMake(volume)(slot)
  }
  private[models] override val src = "slot JOIN " + Volume.src + " ON slot.volume = volume.id"
  private[this] def rowVolume(volume : Volume) = columns.map(makeVolume(volume) _)

  /** Retrieve an individual Slot.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Slot] =
    SELECT("WHERE slot.id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()
  /** Retrieve a list of slots within th egiven volume. */
  private[models] def getVolume(volume : Volume)(implicit db : Site.DB) : Seq[Slot] =
    SQL("SELECT " + columns.select + " FROM slot WHERE volume = {volume} ORDER BY date").
      on('volume -> volume.id).list(rowVolume(volume))
    
  /** Create a new slot in the specified volume. */
  def create(volume : Volume, consent : Consent.Value, date : Date)(implicit site : Site) : Slot = {
    val id = Audit.add(table, SQLArgs('volume -> volume.id, 'consent -> maybe(consent, Consent.NONE), 'date -> date), "id").single(scalar[Id])
    new Slot(id, volume, consent, date)
  }
}
