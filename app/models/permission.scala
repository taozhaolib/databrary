package models

import scala.util.control.Exception.catching
import play.api.i18n.Messages
import play.api.libs.json
import macros._
import dbrary._
import site._

/** The possible levels of permission used for site access, user delegation, and volume permissions.
  * Must match the corresponding postgres "permission" type. */
object Permission extends PGEnum("permission") {
  val NONE, PUBLIC, SHARED, READ, EDIT, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  /** Level at which things become visible. */
  def VIEW = PUBLIC
  /** Alias for EDIT. */
  def CONTRIBUTE = EDIT
  /** Alias for READ. Grants full access to private data, bypassing consent permissions. */
  def PRIVATE = READ

  def message(v : Value, kind : String, args : Any*) : Option[String] = {
    val m = "auth." + kind + "." + v.toString
    if (Messages.isDefinedAt(m)) Some(Messages(m, args : _*)) else None
  }

  implicit val truth : Truth[Value] = Truth[Value](_ != NONE)
  override implicit val sqlType : SQLType[Value] =
    SQLType.transform[Option[String], Value]("permission", classOf[Value])(
      _.fold[Option[Value]](Some(NONE))(withNameOpt),
      p => Some(p.toString))

  def check(has : Value, need : Value)(implicit site : Site) : Boolean =
    has >= need || site.superuser

  /** The necessary permission level to read a data object with the given classification.
    * Equivalent to the SQL function read_permission. */
  def read(t : Classification.Value, c : Consent.Value) : Value =
    if (t == Classification.PRIVATE)
      PRIVATE
    else if (t == Classification.PUBLIC || c >= Consent.PUBLIC)
      PUBLIC
    else if (t == Classification.SHARED || c >= Consent.SHARED)
      SHARED
    else
      PRIVATE

  final val publicDateFields = Array(org.joda.time.DateTimeFieldType.year)
}

trait HasPermission extends PerSite {
  self =>
  def permission : Permission.Value
  final def checkPermission(need : Permission.Value) : Boolean =
    Permission.check(permission, need)(site)
  /** The effective permission for data objects with the given attributes, effectively collapsing selective read permissions to READ or NONE. */
  def dataPermission(classification : Classification.Value, consent : Consent.Value) : HasPermission = new HasPermission {
    def site = self.site
    val permission = {
      val p = self.permission
      if (p >= Permission.READ)
        p
      else if (p >= Permission.read(classification, consent))
        Permission.READ
      else
        Permission.NONE
    }
  }
}

/** The possible levels of participant consent governing [Classification.IDENTIFIED] data.
  * Must match the corresponding postgres "consent" type, except for the NONE value which represents NULL (missing) as this is a common possibility. */
object Consent extends PGEnum("consent") {
  val NONE, PRIVATE, SHARED, EXCERPTS, PUBLIC = Value
  def message(v : Value) : Option[String] = {
    val m = "consent." + v.toString
    if (Messages.isDefinedAt(m)) Some(Messages(m)) else None
  }
  implicit val truth : Truth[Value] = Truth[Value](_ != NONE)
  override implicit val sqlType : SQLType[Value] =
    SQLType.transform[Option[String], Value]("consent", classOf[Value])(
      _.fold[Option[Value]](Some(NONE))(withNameOpt),
      Maybe(_).opt.map(_.toString))
  override implicit val jsonFormat : json.Format[Value] = new json.Format[Value] {
    def writes(v : Value) =
      if (v == NONE) json.JsNull else json.JsNumber(v.id)
    def reads(j : json.JsValue) = j match {
      case json.JsNull => json.JsSuccess(NONE)
      case json.JsNumber(i) if i.isValidInt && i >= 0 && i < maxId => json.JsSuccess(apply(i.toInt))
      case json.JsString(s) => fromString(s).fold[json.JsResult[Value]](json.JsError("error.expected.jsnumber"))(json.JsSuccess(_))
      case _ => json.JsError("error.expected.jsnumber")
    }
  }
}

/** The possible types of data sensitivity according to the presence of identifying user data.
  * Must match the corresponding postgres "consent" type. */
object Classification extends PGEnum("classification") {
  val PRIVATE, RESTRICTED, SHARED, PUBLIC = Value
  def IDENTIFIED = RESTRICTED

  def message(v : Value) : Option[String] = {
    val m = "classification." + v.toString
    if (Messages.isDefinedAt(m)) Some(Messages(m)) else None
  }

  /** The most restrictive data classification level that the current user may access under the given permission and consent level.
    * Equivalent to the SQL function read_classification. */
  def read(p : Permission.Value, c : Consent.Value) : Option[Value] =
    if (p >= Permission.PRIVATE)
      Some(PRIVATE)
    else if (p >= Permission.SHARED)
      if (c >= Consent.SHARED)
        Some(RESTRICTED)
      else
        Some(SHARED)
    else if (p >= Permission.PUBLIC)
      if (c >= Consent.PUBLIC)
        Some(RESTRICTED)
      else
        Some(SHARED)
    else
      None
}
