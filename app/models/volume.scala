package models

import anorm._
import anorm.SqlParser.scalar
import java.sql.Date
import dbrary._
import dbrary.Anorm._
import util._

/** Main organizational unit or package of data, within which everything else exists.
  * Usually represents a single project or dataset with a single set of procedures.
  * @param permission the effective permission level granted to the current user, making this and many other related objects unique to a particular account/request. This will never be less than [[Permission.VIEW]] except possibly for transient objects, as unavailable volumes should never be returned in the first place. */
final class Volume private (val id : Volume.Id, name_ : String, body_ : Option[String], override val permission : Permission.Value) extends TableRowId[Volume] with SitePage with AnnotatedInVolume {
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
    val args = 
    Audit.change("volume", SQLArgs('name -> name, 'body -> body), SQLArgs('id -> id)).execute()
    _name = name
    _body = body
  }

  /** List of parties which have at least the specified level of access to this volume. */
  def partyAccess(p : Permission.Value = Permission.NONE)(implicit db : Site.DB) = VolumeAccess.getParties(this, p)

  /** List of containers within this volume. */
  def containers(implicit db : Site.DB) : Seq[Container] = Container.getVolume(this)

  /** List of toplevel assets within this volume. */
  def toplevelAssets(implicit db : Site.DB) : Seq[SlotAsset] = SlotAsset.getToplevel(this)

  /** Get volume creation information */
  def creationAudit(implicit db : Site.DB) : Option[Audit[Unit]] = {
    def cols = Audit.row[Unit]((), "audit_volume")
    SQL("SELECT " + cols.select + " FROM audit_volume WHERE id = {id} AND action = 'add'").
      on('id -> id).singleOpt(cols)
  }

  /** List of records associated with any slot in this volume.
    * @param category restrict to the specified category
    * @return unique records sorted by category */
  def slotRecords(category : Option[RecordCategory] = None)(implicit db : Site.DB) = Record.getVolume(this, category)

  private[models] def annotatedLevel = "volume"
  private[models] def annotatedId = id

  def pageName(implicit site : Site) = name
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Volume.view(id).url
}

object Volume extends TableId[Volume]("volume") {
  private val permission = "volume_access_check(volume.id, {identity})"
  private[models] val condition = permission + " >= 'VIEW'"
  private[models] val row = Columns[
    Id,  String, Option[String], Option[Permission.Value]](
    'id, 'name,  'body,   SelectAs(permission, "permission")) map {
    (id, name, body, permission) => new Volume(id, name, body, permission.getOrElse(Permission.NONE))
  }

  /** Retrieve an individual Volume.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Option[Volume] =
    SELECT("WHERE id = {id} AND", condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt()

  /** Retrieve the set of all volumes in the system.
    * This only returns volumes for which the current user has [[Permission.VIEW]] access. */
  def getAll(implicit site : Site) : Seq[Volume] =
    SELECT("WHERE", condition).
      on('identity -> site.identity.id).list()
    
  /** Create a new, empty volume with no permissions.
    * The caller should probably add a [[VolumeAccess]] for this volume to grant [[Permission.ADMIN]] access to some user. */
  def create(name : String, body : Option[String] = None)(implicit site : Site) : Volume = {
    val id = Audit.add(table, SQLArgs('name -> name, 'body -> body), "id").single(scalar[Id])
    new Volume(id, name, body, Permission.NONE)
  }
}

trait InVolume {
  def volumeId : Volume.Id = volume.id
  def volume : Volume
  /** Permission granted to the current site user for this object, defined by the containing volume and determined at lookup time. */
  def permission : Permission.Value = volume.permission
}
