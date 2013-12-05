package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** Main organizational unit or package of data, within which everything else exists.
  * Usually represents a single project or dataset with a single set of procedures.
  * @param permission the effective permission level granted to the current user, making this and many other related objects unique to a particular account/request. This will never be less than [[Permission.VIEW]] except possibly for transient objects, as unavailable volumes should never be returned in the first place. */
final class Volume private (val id : Volume.Id, name_ : String, body_ : Option[String], val permission : Permission.Value, val creation : Timestamp)(implicit override val site : Site) extends TableRowId[Volume] with SiteObject with InVolume with JsonableRecord {
  private[this] var _name = name_
  /** Title headline of this volume. */
  def name = _name
  private[this] var _body = body_
  /** Longer, abstract-like description of this volume. */
  def body = _body
  def volume = this

  /** Update the given values in the database and this object in-place. */
  def change(name : String = _name, body : Option[String] = _body) : Future[Boolean] = {
    if (name == _name && body == _body)
      return Async(true)
    Audit.change("volume", SQLTerms('name -> name, 'body -> body), SQLTerms('id -> id))
      .execute.andThen { case scala.util.Success(true) =>
        _name = name
        _body = body
      }
  }

  /** List of parties access to this volume, sorted by level (ADMIN first). */
  lazy val partyAccess : Future[Seq[VolumeAccess]] = VolumeAccess.getParties(this)

  /** List of containers within this volume, except the top. */
  def containers : Future[Seq[Container]] = Container.getVolume(this)
  /** The master container corresponding to this volume. */
  lazy val topContainer : Future[Container] = Container.getTop(this)
  /** The master slot corresponding to this volume, which serves as a proxy target for many annotations. */
  def topSlot : Future[Slot] = topContainer.map(_.fullSlot)

  /** List of toplevel assets within this volume. */
  lazy val toplevelAssets = SlotAsset.getToplevel(this)

  /** List of records defined in this volume.
    * @param category restrict to the specified category
    * @return records sorted by category */
  def allRecords(category : Option[RecordCategory] = None) = Record.getVolume(this, category)

  /** List of all citations on this volume. */
  lazy val citations = VolumeCitation.getVolume(this)
  def setCitations(list : Seq[VolumeCitation]) = VolumeCitation.setVolume(this, list)

  /** List of all funding on this volume. */
  def funding = VolumeFunding.getVolume(this)
  def setFunding(list : Seq[VolumeFunding]) = VolumeFunding.setVolume(this, list)

  /** The list of comments in this volume. */
  lazy val comments = Comment.getVolume(this)
  /** The list of tags on this volume and their use on the topSlot by the current user. */
  lazy val tags = TagWeight.getVolume(this)

  /** An image-able "asset" that may be used as the volume's thumbnail. */
  def thumb = SlotAsset.getThumb(this)

  private type Session = (Option[Slot],Option[Record])
  private lazy val _sessions : Future[Seq[Session]] =
    Record.getSessions(this)

  /** The list of all sessions and their associated record on this volume. */
  def slotRecords : Future[Seq[(Slot,Seq[Record])]] = _sessions.map { sess =>
    val l = sess.sortBy(_._1.map(s => (s.consent == Consent.PRIVATE) -> s.id.unId))
    val r = l.genericBuilder[(Slot,Seq[Record])]
    @scala.annotation.tailrec def group(l : Seq[Session]) : Seq[(Slot,Seq[Record])] = l.headOption match {
      case None => r.result
      case Some((None, _)) => group(l.tail)
      case Some((Some(k), _)) =>
        val (p, s) = l.span(_._1.get.equals(k)) // safe because sorted
        r += k -> p.flatMap(_._2)
        group(s)
    }
    group(l)
  }

  /** The list of all records and their associated sessions on this volume. */
  def recordSlots : Future[Seq[(RecordCategory,Seq[(Record,Seq[Slot])])]] = _sessions.map { sess =>
    val l = sess.sortBy(_._2.map(r => r.category.map(_.id.unId) -> r.id.unId))
    val r = l.genericBuilder[(Record,Seq[Slot])]
    @scala.annotation.tailrec def group(l : Seq[Session]) : Seq[(Record,Seq[Slot])] = l.headOption match {
      case None => r.result
      case Some((_, Some(k))) if k.category.isDefined =>
        val (p, s) = l.span(_._2.get.equals(k)) // safe because sorted
        r += k -> p.flatMap(_._1)
        group(s)
      case _ => group(l.tail)
    }
    groupBy(group(l), (r : (Record, Seq[Slot])) => r._1.category.get)
  }

  /** Basic summary information on this volume.
    * For now this only includes session (cross participant) information. */
  lazy val summary : Future[Volume.Summary] = _sessions.map { sess =>
    var sessions, shared, ages = 0
    var agemin, agemax = Age(0)
    var agesum = 0
    sess.foreach {
      case (Some(s), Some(r)) if r.category.equals(Some(RecordCategory.Participant)) =>
        sessions = sessions + 1
        if (s.consent >= Consent.SHARED) shared = shared + 1
        s.container.date.flatMap(r.age(_)).foreach { a =>
          if (ages == 0) {
            agemin = a
            agemax = a
          } else {
            agemin = agemin.min(a)
            agemax = agemax.max(a)
          }
          ages = ages + 1
          agesum = agesum + a.days
        }
      case _ => ()
    }
    Volume.Summary(
      sessions = sessions,
      shared = shared,
      agerange = Range(agemin, agemax),
      agemean = Age(if (ages == 0) 0 else agesum / ages))
  }

  def pageName = name
  def pageParent = None
  def pageURL = controllers.routes.Volume.view(id)
  def pageActions = Seq(
    Action("view", controllers.routes.Volume.view(id), Permission.VIEW),
    Action("edit", controllers.routes.Volume.edit(id), Permission.EDIT),
    Action("access", controllers.routes.Volume.admin(id), Permission.ADMIN),
    Action("add file", controllers.routes.Asset.createTop(id), Permission.CONTRIBUTE),
    Action("add session", controllers.routes.Slot.createContainer(id), Permission.CONTRIBUTE),
    Action("add participant", controllers.routes.Record.add(id, RecordCategory.PARTICIPANT), Permission.CONTRIBUTE)
  )

  def json(implicit site : Site) =
    JsonRecord.flatten(id,
      Some('name -> name),
      body.map('body -> _),
      Some('creation -> creation)
    )
}

object Volume extends TableId[Volume]("volume") {
  private val permission = "volume_access_check(volume.id, ?::integer)"
  private[models] val condition = "(" + permission + " >= 'VIEW'::permission OR ?::boolean)"
  private[models] def row(implicit site : Site) = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Option[String]]("body")
    , SelectAs[Permission.Value](permission, "volume_permission")
    , SelectAs[Option[Timestamp]]("volume_creation(volume.id)", "volume_creation")
    ).map {
      (id, name, body, permission, creation) => new Volume(id, name, body, permission, creation.getOrElse(new Timestamp(1357900000000L)))
    }.pushArgs(SQLArgs(site.identity.id))

  private[models] def conditionArgs(implicit site : Site) =
    SQLArgs(site.identity.id, site.superuser)

  /** Retrieve an individual Volume.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Volume]] =
    row.SELECT("WHERE id = ? AND", condition)
      .apply(i +: conditionArgs).singleOpt

  /** Retrieve the set of all volumes in the system.
    * This only returns volumes for which the current user has [[Permission.VIEW]] access. */
  def getAll(implicit site : Site) : Future[Seq[Volume]] =
    row.SELECT("WHERE", condition, "ORDER BY volume.name")
      .apply(conditionArgs).list
    
  /** Create a new, empty volume with no permissions.
    * The caller should probably add a [[VolumeAccess]] for this volume to grant [[Permission.ADMIN]] access to some user. */
  def create(name : String, body : Option[String] = None)(implicit site : Site) : Future[Volume] =
    Audit.add(table, SQLTerms('name -> name, 'body -> body), "id").single(SQLCols[Id])
      .map(new Volume(_, name, body, Permission.NONE, new Timestamp))

  private final val DATABRARY : Id = asId(1)
  /** The "databrary" volume, containing meta-information about Databrary itself, documents, etc.
    * This is expected to be readable by everyone so we bypass permission checks here. */
  final def Databrary(implicit site : Site) : Future[Volume] =
    row.SELECT("WHERE id = ?").apply(DATABRARY).single

  case class Summary(sessions : Int, shared : Int, agerange : Range[Age], agemean : Age)
}

trait InVolume extends HasPermission {
  def volumeId : Volume.Id = volume.id
  def volume : Volume
  implicit def site : Site = volume.site
  /** Permission granted to the current site user for this object, defined by the containing volume and determined at lookup time. */
  def getPermission : Permission.Value = volume.permission
}
