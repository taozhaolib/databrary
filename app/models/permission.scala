package models

import scala.util.control.Exception.catching
import play.api.i18n.Messages
import play.api.libs.json
import macros._
import dbrary._
import site._

/** The possible levels of permission used for site access, user delegation, and volume permissions.
  * Must match the corresponding postgres "permission" type. */
object Permission extends SQL.Enum("permission") {
  val NONE, PUBLIC, SHARED, READ, EDIT, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  /** Level at which things become visible. */
  def VIEW = PUBLIC
  /** Alias for EDIT. */
  def CONTRIBUTE = EDIT
  /** Alias for READ. Grants full access to private data, bypassing release permissions. */
  def PRIVATE = READ

  def message(v : Value, kind : String, args : Any*) : Option[String] = {
    val m = "auth." + kind + "." + v.toString
    if (Messages.isDefinedAt(m)) Some(Messages(m, args : _*)) else None
  }

  implicit val truth : Truth[Value] = Truth[Value](_ != NONE)
  override implicit val sqlType : SQL.Type[Value] =
    new SQL.Type.default(NONE)(
      SQL.Type[Value]("permission", classOf[Value])(withNameOpt, _.toString))

  def check(has : Value, need : Value)(implicit site : Site) : Boolean =
    has >= need || site.superuser

  /** The necessary permission level to read a data object with the given release.
    * Equivalent to the SQL function read_permission. */
  def read(r : Release.Value) : Value =
    if (r >= Release.PUBLIC)
      PUBLIC
    else if (r >= Release.SHARED)
      SHARED
    else
      PRIVATE

  final val publicDateFields = Array(org.joda.time.DateTimeFieldType.year)
}

/** The possible levels of sharing for data.
  * Must match the corresponding postgres "release" type, except for the NONE value which represents NULL (missing) as this is a common case. */
object Release extends SQL.Enum("release") {
  val DEFAULT, PRIVATE, SHARED, EXCERPTS, PUBLIC = Value

  implicit val truth : Truth[Value] = Truth[Value](_ != DEFAULT)
  override implicit val sqlType : SQL.Type[Value] =
    SQL.Type.transform[Option[String],Value]("release", classOf[Value])(
      _.fold[Option[Value]](Some(DEFAULT))(withNameOpt),
      v => Maybe(v).opt(_.toString))
  override implicit object jsonFormat extends json.Format[Value] {
    def writes(v : Value) = json.JsNumber(v.id)
    def reads(j : json.JsValue) = j match {
      case json.JsNull => json.JsSuccess(DEFAULT)
      case json.JsNumber(i) if i.isValidInt && i >= 0 && i < maxId => json.JsSuccess(apply(i.toInt))
      case json.JsString(s) => fromString(s).fold[json.JsResult[Value]](json.JsError("error.expected.jsnumber"))(json.JsSuccess(_))
      case _ => json.JsError("error.expected.jsnumber")
    }
  }

  /** The most restrictive release level that the current user may access under the given permission.
    * Equivalent to the SQL function read_release. */
  def read(p : Permission.Value) : Option[Value] =
    if (p >= Permission.PRIVATE)
      Some(DEFAULT)
    else if (p >= Permission.SHARED)
      Some(SHARED)
    else if (p >= Permission.PUBLIC)
      Some(PUBLIC)
    else
      None

  def message(v : Value, kind : String) : Option[String] = {
    val m = kind + "." + v.toString
    if (Messages.isDefinedAt(m)) Some(Messages(m)) else None
  }
}

trait HasPermission extends PerSite {
  self =>
  def permission : Permission.Value
  final def checkPermission(need : Permission.Value) : Boolean =
    Permission.check(permission, need)(site)
  /** The effective permission for data objects with the given attributes, effectively collapsing ineffective permissions to NONE. */
  def dataPermission(release : Release.Value) : HasPermission = new HasPermission {
    def site = self.site
    val permission = {
      val p = self.permission
      if (p >= Permission.read(release))
        p
      else
        Permission.NONE
    }
  }
}
