package models

import anorm._
import anorm.SqlParser.scalar
import java.sql.{Date,Timestamp}
import dbrary._
import dbrary.Anorm._
import site._

/** Main organizational unit or package of data, within which everything else exists.
  * Usually represents a single project or dataset with a single set of procedures.
  * @param permission the effective permission level granted to the current user, making this and many other related objects unique to a particular account/request. This will never be less than [[Permission.VIEW]] except possibly for transient objects, as unavailable volumes should never be returned in the first place. */
final class Volume private (val id : Volume.Id, name_ : String, body_ : Option[String], override val permission : Permission.Value, val creation : Timestamp) extends TableRowId[Volume] with SitePage with InVolume {
  private[this] var _name = name_
  /** Title headline of this volume. */
  def name = _name
  private[this] var _body = body_
  /** Longer, abstract-like description of this volume. */
  def body = _body
  def volume = this

  /** Update the given values in the database and this object in-place. */
  def change(name : String = _name, body : Option[String] = _body)(implicit site : Site) : Unit = {
    if (name == _name && body == _body)
      return
    Audit.change("volume", SQLArgs('name -> name, 'body -> body), SQLArgs('id -> id)).execute()
    _name = name
    _body = body
  }

  private[this] val _partyAccess = CachedVal[Seq[VolumeAccess], Site.DB](VolumeAccess.getParties(this)(_))
  /** List of parties access to this volume. Cached.
    * @return VolumeAccess sorted by level (ADMIN first). */
  def partyAccess(implicit db : Site.DB) : Seq[VolumeAccess] = _partyAccess

  /** List of containers within this volume. */
  def containers(implicit db : Site.DB) : Seq[Container] = Container.getVolume(this)
  private val _topContainer = CachedVal[Container, Site.DB](Container.getTop(this)(_))
  /** The master container corresponding to this volume. Cached. */
  def topContainer(implicit db : Site.DB) : Container = _topContainer
  /** The master slot corresponding to this volume, which serves as a proxy target for many annotations. */
  def topSlot(implicit db : Site.DB) : Slot = {
    if (_topContainer.isEmpty) {
      val s = Slot.getTop(this)
      (_topContainer() = s.container)._fullSlot() = s
    }
    topContainer.fullSlot
  }

  /** List of toplevel assets within this volume. */
  def toplevelAssets(implicit db : Site.DB) : Seq[SlotAsset] = SlotAsset.getToplevel(this)

  /** Get volume creation information */
  private[this] def creationAudit(implicit db : Site.DB) : Option[Audit[Unit]] =
    Audit.row[Unit]((), "audit_volume").SQL("WHERE id = {id} AND action = 'add'").
      on('id -> id).singleOpt

  /** List of records defined in this volume.
    * @param category restrict to the specified category
    * @return records sorted by category */
  def allRecords(category : Option[RecordCategory] = None)(implicit db : Site.DB) = Record.getVolume(this, category)

  /** List of all citations on this volume. */
  def citations(implicit db : Site.DB) = VolumeCitation.getVolume(this)

  /** List of all funding on this volume. */
  def funding(implicit db : Site.DB) = VolumeFunding.getVolume(this)

  /** The list of comments in this volume. */
  def comments(implicit db : Site.DB) : Seq[Comment] = Comment.getVolume(this)

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = None
  def pageURL(implicit site : Site) = controllers.routes.Volume.view(id)
  def pageActions(implicit site : Site) = Seq(
    ("view", controllers.routes.Volume.view(id), Permission.VIEW),
    ("edit", controllers.routes.Volume.edit(id), Permission.EDIT),
    ("access", controllers.routes.Volume.admin(id), Permission.ADMIN),
    ("add asset", controllers.routes.Asset.create(id, topContainer.id), Permission.CONTRIBUTE),
    ("add slot", controllers.routes.Slot.createContainer(id), Permission.CONTRIBUTE)
  ).filter(a => permission >= a._3)
}

object Volume extends TableId[Volume]("volume") {
  private val permission = "volume_access_check(volume.id, {identity})"
  private[models] val condition = permission + " >= 'VIEW' OR {superuser}"
  private[models] val row = Columns[
    Id,  String, Option[String], Option[Permission.Value],           Option[Timestamp]](
    'id, 'name,  'body,          SelectAs(permission, "permission"), SelectAs("volume_creation(volume.id)", "creation")) map {
    (id, name, body, permission, creation) => new Volume(id, name, body, permission.getOrElse(Permission.NONE), creation.getOrElse(new Timestamp(1357900000000L)))
  }

  /** Retrieve an individual Volume.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Volume] =
    row.SQL("WHERE id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id, 'superuser -> site.superuser).singleOpt()

  /** Retrieve the set of all volumes in the system.
    * This only returns volumes for which the current user has [[Permission.VIEW]] access. */
  def getAll(implicit site : Site) : Seq[Volume] =
    row.SQL("WHERE", condition).
      on('identity -> site.identity.id, 'superuser -> site.superuser).list()
    
  /** Create a new, empty volume with no permissions.
    * The caller should probably add a [[VolumeAccess]] for this volume to grant [[Permission.ADMIN]] access to some user. */
  def create(name : String, body : Option[String] = None)(implicit site : Site) : Volume = {
    val id = Audit.add(table, SQLArgs('name -> name, 'body -> body), "id").single(scalar[Id])
    new Volume(id, name, body, Permission.NONE, new Timestamp(System.currentTimeMillis))
  }
}

trait InVolume {
  def volumeId : Volume.Id = volume.id
  def volume : Volume
  /** Permission granted to the current site user for this object, defined by the containing volume and determined at lookup time. */
  def permission : Permission.Value = volume.permission
}
