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
final class Volume private (val id : Volume.Id, title_ : String, description_ : Option[String], val permission : Permission.Value) extends TableRowId[Volume] with SitePage {
  private[this] var _title = title_
  /** Title headline of this volume. */
  def title = _title
  private[this] var _description = description_
  /** Longer, abstract-like description of this volume. */
  def description = _description

  /** Update the given values in the database and this object in-place. */
  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    val args = 
    Audit.change("volume", SQLArgs('title -> title, 'description -> description), SQLArgs('id -> id)).execute()
    _title = title
    _description = description
  }

  def pageName(implicit site : Site) = title
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Volume.view(id).url

  /** List of parties which have at least the specified level of access to this volume. */
  def partyAccess(p : Permission.Value = Permission.NONE)(implicit db : Site.DB) = VolumeAccess.getParties(this, p)

  /** List of containers within this volume. */
  def containers(implicit db : Site.DB) = Container.getVolume(this)

  /** Get volume creation information */
  def creationAudit(implicit db : Site.DB) : Option[Audit[Unit]] = {
    def cols = Audit.row[Unit]((), "audit_volume")
    SQL("SELECT " + cols.select + " FROM audit_volume WHERE id = {id} AND action = 'add'").
      on('id -> id).singleOpt(cols)
  }

  /** List of records associated with any slot in this volume.
    * @param category restrict to the specified category
    * @return unique records sorted by category */
  def slotRecords(category : Option[RecordCategory] = None)(implicit db : Site.DB) = Record.getSlots(this, category)
}

object Volume extends TableId[Volume]("volume") {
  private[models] val row = Columns[
    Id,  String, Option[String], Option[Permission.Value]](
    'id, 'title, 'description,   SelectAs(permission, "permission")) map {
    (id, title, description, permission) => new Volume(id, title, description, permission.getOrElse(Permission.NONE))
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
  def create(title : String, description : Option[String] = None)(implicit site : Site) : Volume = {
    val id = Audit.add(table, SQLArgs('title -> title, 'description -> description), "id").single(scalar[Id])
    new Volume(id, title, description, Permission.NONE)
  }
}

