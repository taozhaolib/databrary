package models

import scala.concurrent.{ExecutionContext,Future}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{Json,JsValue,JsObject,JsNull}
import macros._
import dbrary._
import site._

/** Main organizational unit or package of data, within which everything else exists.
  * Usually represents a single project or dataset with a single set of procedures.
  * @param permission the effective permission level granted to the current user, making this and many other related objects unique to a particular account/request. This will never be less than [[Permission.VIEW]] except possibly for transient objects, as unavailable volumes should never be returned in the first place. */
final class Volume private (val id : Volume.Id, private[this] var name_ : String, private[this] var alias_ : Option[String], private[this] var body_ : Option[String], override val permission : Permission.Value, val creation : Timestamp)(implicit override val site : Site) extends TableRowId[Volume] with SiteObject with InVolume {
  /** Title headline. */
  def name = name_
  /** Short, internal name, only available to editors. */
  def alias = alias_.filter(_ => checkPermission(Permission.EDIT))
  /** Longer, abstract-like description. */
  def body = body_
  def volume = this

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[String] = None, alias : Option[Option[String]] = None, body : Option[Option[String]] = None) : Future[Boolean] =
    Audit.change("volume", SQLTerms.flatten(
        name.map('name -> _),
        alias.map('alias -> _),
        body.map('body -> _)),
      sqlKey)
      .execute.andThen { case scala.util.Success(true) =>
        name.foreach(name_ = _)
        alias.foreach(alias_ = _)
        body.foreach(body_ = _)
      }

  /** List of parties access to this volume, sorted by level (ADMIN first). */
  def partyAccess(access : Permission.Value = Permission.NONE) : Future[Seq[VolumeAccess]] = VolumeAccess.getParties(this, access)

  /** List of containers within this volume. */
  def containers : Future[Seq[Container]] = Volume.Containers.get(this)
  /** The master container corresponding to this volume, which serves as a proxy target for many annotations. */
  def top : Future[Container] = Container.getTop(this)

  /** List of toplevel assets within this volume. */
  def excerpts = Excerpt.getVolume(this)

  private val _records : FutureVar[Seq[Record]] = FutureVar(Record.getVolume(this))
  def records : Future[Seq[Record]] = _records()

  /** Corresponding citation for this volume. */
  private val _citation : FutureVar[Option[Citation]] =
    FutureVar(VolumeCitation.get(this))
  def citation : Future[Option[Citation]] = _citation()
  def setCitation(cite : Option[Citation]) : Future[Boolean] =
    VolumeCitation.set(this, cite)
    .andThen { case scala.util.Success(true) =>
      _citation.set(cite)
    }
  def funding : Future[Seq[Funding]] = VolumeFunding.get(this)

  /** The list of comments in this volume. */
  def comments = Comment.getVolume(this)
  /** The list of tags on this volume and their use on the topSlot by the current user. */
  def tags = TagWeight.getVolume(this)

  /** An image-able "asset" that may be used as the volume's thumbnail. */
  def thumb = Excerpt.getVolumeThumb(this)

  /** Volumes ("datasets") which provide data included in this volume. */
  def providers : Future[Seq[Volume]] =
    Volume.row.SQL("SELECT DISTINCT ON (volume.id) " + _ + " FROM " + _ + """
      JOIN container ON volume.id = container.volume
      JOIN volume_inclusion ON container.id = volume_inclusion.container
      WHERE volume_inclusion.volume = ? AND """ + Volume.condition)
    .apply(id).list

  /** Volumes ("studies") which include data provided by this volume. */
  def consumers : Future[Seq[Volume]] =
    Volume.row.SQL("SELECT DISTINCT ON (volume.id) " + _ + " FROM " + _ + """
      JOIN volume_inclusion ON volume.id = volume_inclusion.volume
      JOIN container ON volume_inclusion.container = container.id
      WHERE container.volume = ? AND """ + Volume.condition)
    .apply(id).list

  /** List of parties through whom the current user has the given access to this volume. */
  def adminAccessVia : Future[Seq[SiteParty]] =
    SiteParty.row
    .SELECT("JOIN volume_access ON party.id = volume_access.party",
      "WHERE volume_access.volume = ? AND authorize_view.member = 'ADMIN' AND volume_access.individual = 'ADMIN' AND (authorize_view.site = 'ADMIN' OR volume_access.children = 'ADMIN')")
    .apply(id).list

  final def auditDownload(implicit site : Site) : Future[Boolean] =
    Audit.download("volume", 'id -> id)

  def pageName = alias.getOrElse(name)
  def pageParent = None
  def pageURL = controllers.routes.VolumeHtml.view(id, None)

  lazy val fileName : Future[String] = {
    def last(s : String) =
      Maybe(s.lastIndexOf(' ')).fold(s)(i => s.substring(i+1))
    for {
      owns <- partyAccess(Permission.ADMIN)
      own = owns.headOption.map(_.party.name).map(last _)
      cite <- citation
      auth = cite.flatMap(_.authors).flatMap(_.headOption).map(last _).filterNot(a => own.exists(_.equals(a)))
      nme = store.truncate(alias.getOrElse(name))
    } yield {
      store.fileName(Seq("databrary" + id) ++
        own ++ auth ++ cite.flatMap(_.year).map(_.toString) :+ nme : _*)
    }
  }

  lazy val json : JsonRecord =
    JsonRecord.flatten(id
    , Some('name -> name)
    , alias.map('alias -> _)
    , Some('body -> body)
    , Some('creation -> creation)
    , Some('permission -> permission)
    , _citation.peek.map(c => 'citation -> c.map(_.json))
    )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json,
      /* this could be generalized for all fields in json when there's overlap: */
      if (_citation.peek.isDefined) options - "citation" else options,
      ("access", opt => partyAccess(opt.headOption.flatMap(Permission.fromString(_)).getOrElse(Permission.NONE))
        .map(JsonArray.map(_.json - "volume"))),
      ("citation", opt => citation.map(_.fold[JsValue](JsNull)(_.json.js))),
      ("links", opt => VolumeLink.get(this).map(JsonArray.map(_.json))),
      ("funding", opt => funding.map(JsonArray.map(_.json))),
      ("comments", opt => comments.map(JsonArray.map(_.json))),
      ("tags", opt => tags.map(JsonRecord.map(_.json))),
      ("records", opt => records.map(JsonArray.map(_.json - "volume"))),
      ("containers", opt => containers.map(JsonArray.map { c =>
        c.json - "volume" ++ c._jsonRecords(false).peek.map[JsonField]('records -> _)
      })),
      ("excerpts", opt => excerpts.map(JsonArray.map(_.json))),
      ("top", opt => top.map(t => (t.json - "volume").obj)),
      ("consumers", opt => consumers.map(JsonRecord.map(_.json))),
      ("providers", opt => providers.map(JsonRecord.map(_.json)))
    )
}

object Volume extends TableId[Volume]("volume") {
  private object Permission extends Table[models.Permission.Value]("volume_permission") {
    private val columns = Columns(
        SelectColumn[models.Permission.Value]("permission")
      ).from("LATERAL (VALUES (volume_access_check(volume.id, ?::integer), ?::boolean)) AS " + _ + " (permission, superuser)")
    def row(implicit site : Site) =
      columns.pushArgs(SQLArgs(site.identity.id, site.superuser))
    def condition =
      "(" + table + ".permission >= 'PUBLIC'::permission OR " + table + ".superuser)"
  }
  private[models] val condition = Permission.condition

  private final val defaultCreation = new Timestamp(1357900000000L)
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Option[String]]("alias")
    , SelectColumn[Option[String]]("body")
    , SelectAs[Option[Timestamp]]("volume_creation(volume.id)", "volume_creation")
    ).map { (id, name, alias, body, creation) =>
      (permission : models.Permission.Value, site : Site) =>
        new Volume(id, name, alias, body, permission, creation.getOrElse(defaultCreation))(site)
    }
  private[models] def row(implicit site : Site) =
    columns.*(Permission.row).map { case (v, p) => v(p, site) }

  /** Retrieve an individual Volume.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Volume]] =
    row.SELECT("WHERE id = ? AND", condition)
      .apply(i).singleOpt

  def search(query : Option[String], party : Option[Party.Id])(implicit site : Site) : Future[Seq[Volume]] =
    row.SELECT(
      party.fold("")(_ => "JOIN volume_access ON volume.id = volume_access.volume"),
      query.fold("")(_ => "JOIN volume_text_idx ON volume.id = volume_text_idx.volume, plainto_tsquery('english', ?) query"),
      "WHERE volume.id > 0 AND",
      party.fold("")(_ => "volume_access.party = ? AND volume_access.individual >= 'EDIT' AND"),
      query.fold("")(_ => "ts @@ query AND"),
      condition,
      "ORDER BY",
      query.fold("")(_ => "ts_rank(ts, query) DESC, "),
      party.fold("")(_ => "individual DESC,"),
      "volume.id")
    .apply(SQLArgs.opt(query) ++ party.map(SQLArg(_))).list

  /** Create a new, empty volume with no permissions.
    * The caller should probably add a [[VolumeAccess]] for this volume to grant [[Permission.ADMIN]] access to some user. */
  def create(name : String, alias : Option[String], body : Option[String] = None)(implicit site : Site) : Future[Volume] =
    Audit.add(table, SQLTerms('name -> name, 'alias -> alias, 'body -> body), "id")
      .single(SQLCols[Id].map(new Volume(_, name, alias, body, models.Permission.NONE, new Timestamp)))

  private final val CORE : Id = asId(0)
  /** The "core" volume, containing site-wide "global" assets.
    * We ignore any access rules here and grant everyone DOWNLOAD. */
  final def Core(implicit site : Site) : Volume =
    new Volume(CORE, "core", None, None, models.Permission.SHARED, defaultCreation)

  object Containers {
    def get(vol : Volume) : Future[Seq[Container]] =
      vol._records.peek.fold(Container.getVolume(vol)) { records =>
        Container.rowVolume(vol).leftJoin(
          SlotRecord.columns ~+ SelectColumn[Record.Id]("slot_record", "record"),
          "container.id = slot_record.container")
        .SELECT(/* should be: "ORDER BY container.id"*/).apply().list
        .map(fill(records, _))
      }

    private def fill(records : Seq[Record], l : Seq[(Container, Option[(Segment, Record.Id)])]) : Seq[Container] = {
      if (l.isEmpty)
        return Nil
      val rm = TableIdMap(records : _*)
      val r = l.genericBuilder[Container]
      var cc : Container = l.head._1
      val cr = Seq.newBuilder[(Segment, Record)]
      def next() {
        cc._records.set(cr.result)
        cr.clear
        r += cc
      }
      for ((c, or) <- l) {
        if (!(cc.id === c.id)) {
          next()
          cc = c
        }
        or foreach { case (seg, r) =>
          rm.get(r) foreach { rec =>
            cr += seg -> rec
          }
        }
      }
      next()
      r.result
    }
  }

  def updateIndex(implicit defaultContext : ExecutionContext) : Future[Boolean] =
    SQL("SELECT volume_text_refresh()").apply().execute
}

trait InVolume extends HasPermission {
  def volumeId : Volume.Id = volume.id
  def volume : Volume
  implicit def site : Site = volume.site
  /** Permission granted to the current site user for this object, defined by the containing volume and determined at lookup time. */
  def permission : Permission.Value = volume.permission
}
