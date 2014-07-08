package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{Json,JsValue,JsObject,JsNull}
import macros._
import dbrary._
import site._

/** Main organizational unit or package of data, within which everything else exists.
  * Usually represents a single project or dataset with a single set of procedures.
  * @param permission the effective permission level granted to the current user, making this and many other related objects unique to a particular account/request. This will never be less than [[Permission.VIEW]] except possibly for transient objects, as unavailable volumes should never be returned in the first place. */
final class Volume private (val id : Volume.Id, name_ : String, alias_ : Option[String], body_ : Option[String], override val permission : Permission.Value, val creation : Timestamp)(implicit override val site : Site) extends TableRowId[Volume] with SiteObject with InVolume {
  private[this] var _name = name_
  /** Title headline. */
  def name = _name
  private[this] var _alias = alias_
  /** Short, internal name, only available to editors. */
  def alias = _alias.filter(_ => checkPermission(Permission.EDIT))
  private[this] var _body = body_
  /** Longer, abstract-like description. */
  def body = _body
  def volume = this

  /** Update the given values in the database and this object in-place. */
  def change(name : Option[String] = None, alias : Option[Option[String]] = None, body : Option[Option[String]] = None) : Future[Boolean] =
    Audit.change("volume", SQLTerms.flatten(
	name.map('name -> _),
	alias.map('alias -> _),
	body.map('body -> _)),
      sqlKey)
      .execute.andThen { case scala.util.Success(true) =>
        name.foreach(_name = _)
        body.foreach(_body = _)
      }

  /** List of parties access to this volume, sorted by level (ADMIN first). */
  def partyAccess(access : Permission.Value = Permission.NONE) : Future[Seq[VolumeAccess]] = VolumeAccess.getParties(this, access)

  /** List of containers within this volume, except the top. */
  def containers : Future[Seq[Container]] = Container.getVolume(this)
  /** The master container corresponding to this volume, which serves as a proxy target for many annotations. */
  def top : Future[Container] = Container.getTop(this)

  /** List of toplevel assets within this volume. */
  def excerpts = SlotAsset.getExcerpts(this)

  /** List of records defined in this volume.
    * @param category restrict to the specified category
    * @return records sorted by category */
  def records(category : Option[RecordCategory] = None) = Record.getVolume(this, category)

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
  def thumb = SlotAsset.getThumb(this)

  private[this] lazy val _sessions : Future[Seq[Volume.Session]] =
    Volume.Session.get(this)

  lazy val sessions : Future[Seq[Volume.Session.Group]] = _sessions.map(Volume.Session.group)

  private[this] type SessionRecord = (Record, Seq[Slot])
  /** The list of all records and their associated sessions on this volume. */
  private[this] lazy val recordSlots : Future[Seq[SessionRecord]] = _sessions.map { sess =>
    val l = sess.sortBy(_._2.map { case (_, r) => r.category.map(_.id.unId) -> r.id.unId })
    val r = l.genericBuilder[(Record,Seq[Slot])]
    @scala.annotation.tailrec def group(l : Seq[Volume.Session]) {
      l.headOption match {
	case None =>
	case Some((c, None)) =>
	  group(l.tail)
	case Some((c, Some((seg, rec)))) =>
	  val (p, s) = l.span(_._2.exists(_._2 === rec))
	  r += rec -> p.map {
	    case (cont, Some((seg, _))) => cont * seg
	    case (cont, None) => cont // impossible
	  }
	  group(s)
      }
    }
    group(l)
    r.result
  }

  def recordCategorySlots : Future[Seq[(RecordCategory,Seq[SessionRecord])]] =
    recordSlots.map(rs =>
      groupBy(rs.dropWhile(_._1.category.isEmpty), (sr : SessionRecord) => sr._1.category.get))

  /** Basic summary information on this volume.
    * For now this only includes session (cross participant) information. */
  lazy val summary : Future[Volume.Summary] = _sessions.map { sess =>
    var sessions, shared, ages = 0
    var agemin, agemax = Age(0)
    var agesum = 0
    sess.foreach {
      case (s, Some((_, r))) if r.category.exists(_ === RecordCategory.Participant) =>
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

  /** Volumes ("datasets") which provide data included in this volume. */
  def providers : Future[Seq[Volume]] =
    Volume.row.SQL("SELECT DISTINCT ON (volume.id) " + _ + " FROM " + _ + """
      JOIN container ON volume.id = container.volume
      JOIN volume_inclusion ON container.id = volume_inclusion.container
      WHERE volume_inclusion.volume = ?""")
    .apply(id).list

  /** Volumes ("studies") which include data provided by this volume. */
  def consumers : Future[Seq[Volume]] =
    Volume.row.SQL("SELECT DISTINCT ON (volume.id) " + _ + " FROM " + _ + """
      JOIN volume_inclusion ON volume.id = volume_inclusion.volume
      JOIN container ON volume_inclusion.container = container.id
      WHERE container.volume = ?""")
    .apply(id).list

  /** List of parties through whom the current user has the given access to this volume. */
  def adminAccessVia : Future[Seq[SiteParty]] =
    SiteParty.row
    .SELECT("JOIN volume_access ON party.id = volume_access.party",
      "WHERE volume_access.volume = ? AND authorize_view.member = 'ADMIN' AND volume_access.individual = 'ADMIN' AND (authorize_view.site = 'ADMIN' OR volume_access.children = 'ADMIN')")
    .apply(id).list

  def pageName = alias.getOrElse(name)
  def pageParent = None
  def pageURL = controllers.routes.VolumeHtml.view(id)

  lazy val json : JsonRecord =
    JsonRecord.flatten(id,
      Some('name -> name),
      alias.map('alias -> _),
      Some('body -> body),
      Some('creation -> creation),
      Some('permission -> permission)
    )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options,
      ("summary", opt => summary.map(_.json.js)),
      ("access", opt => partyAccess(opt.headOption.flatMap(Permission.fromString(_)).getOrElse(Permission.NONE))
	.map(JsonArray.map(_.json - "volume"))),
      ("citation", opt => citation.map(_.fold[JsValue](JsNull)(_.json.js))),
      ("funding", opt => funding.map(JsonArray.map(_.json))),
      ("comments", opt => comments.map(JsonArray.map(_.json))),
      ("tags", opt => tags.map(JsonRecord.map(_.json))),
      ("categories", opt => recordCategorySlots.map(l =>
	JsObject(l.map { case (c, rl) => (c.id.toString, Json.toJson(rl.map(_._1.id))) }))),
      ("records", opt => recordSlots.map(JsonRecord.map { case (r, ss) =>
        r.json - "volume"
      })),
      ("sessions", opt => sessions.map(JsonRecord.map { case (cont, crs) =>
	cont.json - "volume" + ('categories -> JsObject(crs.map { case (cat, rs) =>
	  (cat.fold("")(_.toString), JsonArray.map[(Segment, Record), JsonRecord] { case (seg, rec) =>
	    JsonRecord.flatten(rec.id
	    , if (seg.isFull) None else Some('segment -> seg)
	    , rec.age(cont).map('age -> _)
	    )
	  }(rs))
	}))
      })),
      ("excerpts", opt => excerpts.map(JsonArray.map(_.json))),
      ("top", opt => top.map(t => (t.json - "volume" - "top").obj)),
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
  private[models] def row(implicit site : Site) = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("name")
    , SelectColumn[Option[String]]("alias")
    , SelectColumn[Option[String]]("body")
    , SelectAs[Option[Timestamp]]("volume_creation(volume.id)", "volume_creation")
    ).*(Permission.row)
      .map { case ((id, name, alias, body, creation), permission) =>
	new Volume(id, name, alias, body, permission, creation.getOrElse(defaultCreation))
      }

  /** Retrieve an individual Volume.
    * This checks user permissions and returns None if the user lacks [[Permission.VIEW]] access. */
  def get(i : Id)(implicit site : Site) : Future[Option[Volume]] =
    row.SELECT("WHERE id = ? AND", condition)
      .apply(i).singleOpt

  def search(query : Option[String], party : Option[Party.Id])(implicit site : Site) : Future[Seq[Volume]] =
    /* XXX ts indexes! */
    row.SELECT(
      party.fold("")(_ => "JOIN volume_access ON volume.id = volume_access.volume"),
      "WHERE volume.id > 0 AND",
      party.fold("")(_ => "volume_access.party = ? AND volume_access.individual >= 'EDIT' AND"),
      query.fold("")(_ => "to_tsvector(name || ' ' || coalesce(body, '')) @@ plainto_tsquery(?) AND"),
      condition,
      "ORDER BY",
      query.fold("")(_ => "ts_rank(to_tsvector(name || ' ' || coalesce(body, '')), plainto_tsquery(?)),"),
      party.fold("")(_ => "individual DESC,"),
      "volume.id")
    .apply(party.fold(SQLArgs())(SQLArgs(_)) ++ query.fold(SQLArgs())(q => SQLArgs(q, q))).list

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

  case class Summary(sessions : Int, shared : Int, agerange : Range[Age], agemean : Age) {
    lazy val json = JsonObject(
      'sessions -> sessions,
      'shared -> shared,
      'agerange -> agerange,
      'agemean -> agemean
    )
  }

  type Session = (ContextSlot, Option[(Segment, Record)])

  object Session {
    private val base =
      SlotConsent.row.from(
      """(SELECT id AS container, COALESCE(segment, '(,)'::segment) AS segment, consent
           FROM container LEFT JOIN slot_consent ON id = container
	  WHERE volume = ?
	UNION ALL
	  SELECT s.container, COALESCE(c.segment, '(,)'::segment) * s.segment AS segment, c.consent
	    FROM volume_inclusion s LEFT JOIN slot_consent c
	      ON s.container = c.container AND s.segment && c.segment
	   WHERE volume = ?
	) AS """ + _)
    private def baseVolume(vol : Volume) =
      base.pushArgs(SQLArgs(vol.id, vol.id))
    private def columnsVolume(vol : Volume) =
      baseVolume(vol)
      .join(Container.columns.map(_(vol)), "slot_consent.container = container.id")
      .map(tupleApply)

    private def row(vol : Volume) : Selector[Session] =
      columnsVolume(vol)
      .leftJoin(SlotRecord.columns
	.join(Record.sessionRow(vol), "slot_record.record = record.id"),
	"container.id = slot_record.container AND slot_consent.segment && slot_record.segment AND (record.volume = container.volume OR record.volume = ?)")
      .pushArgs(SQLArgs(vol.id))
      .map {
	case (slot, None) => (slot, None)
	case (slot, Some((seg, rec))) => (slot, Some((seg, rec(slot.consent))))
      }

    private[Volume] def get(vol : Volume) : Future[Seq[Session]] =
      row(vol)
      .SELECT("ORDER BY container.top DESC, container.id, record.category NULLS LAST, record.id")
      .apply().list

    type Group = (Container, Seq[(Option[RecordCategory.Id], Seq[(Segment, Record)])])
    private[Volume] def group(l : Seq[Session]) : Seq[Group] = {
      if (l.isEmpty)
	return Nil
      val r = l.genericBuilder[Group]
      var c1 : Container = l.head._1.container
      val r1 = Seq.newBuilder[(Option[RecordCategory.Id], Seq[(Segment, Record)])]
      var c2 : Option[Option[RecordCategory.Id]] = None
      val r2 = Seq.newBuilder[(Segment, Record)]
      val rc = Seq.newBuilder[Record]
      def next2(n2 : Option[Option[RecordCategory.Id]]) {
	c2.foreach { c2s =>
	  r1 += c2s -> r2.result
	  r2.clear
	}
	c2 = n2
      }
      def next1() {
	next2(None)
	c1._records.set(rc.result)
	rc.clear
	r += c1 -> r1.result
	r1.clear
      }
      for ((c, or) <- l) {
	if (!(c1.id === c.containerId)) {
	  next1()
	  c1 = c.container
	}
	or foreach { case (seg, rec) =>
	  val cat = rec.categoryId
	  if (!c2.exists(_.equals(cat)))
	    next2(Some(cat))
	  r2 += c.segment * seg -> rec
	  rc += rec
	}
      }
      next1()
      r.result
    }
  }

}

trait InVolume extends HasPermission {
  def volumeId : Volume.Id = volume.id
  def volume : Volume
  implicit def site : Site = volume.site
  /** Permission granted to the current site user for this object, defined by the containing volume and determined at lookup time. */
  def permission : Permission.Value = volume.permission
}
