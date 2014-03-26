package models

import scala.util.control.Exception.catching
import play.api.i18n.Messages
import macros._
import dbrary._
import site._

/** The possible levels of permission used for site access, user delegation, and volume permissions.
  * Must match the corresponding postgres "permission" type. */
object Permission extends PGEnum("permission") {
  val NONE, VIEW, DOWNLOAD, CONTRIBUTE, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  /** Alias for ADMIN. Volume ADMINs are considered OWNers. */
  def OWN = ADMIN
  /** Alias for CONTRIBUTE. */
  def EDIT = CONTRIBUTE
  /** Alias for CONTRIBUTE. Grants full access to data, bypassing consent permissions. */
  def FULL = CONTRIBUTE
  /** Alias for DOWNLOAD. DOWNLOAD permissions grant access to shared data, while non-data only requires VIEW. */
  def DATA = DOWNLOAD

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

  /** The effective permission for data objects with the given attributes.
    * Note that this is also implemented (and used) in the SQL function data_permission, which must be kept consistent. */
  def data(p : Value, consent : Consent.Value, classification : Classification.Value, top : Boolean = false)(implicit site_ : Site) : HasPermission = new HasPermission {
    val site = site_
    val permission =
      if (p >= FULL)
        p
      else if (p >= VIEW)
        if (Classification.download(p, consent, top).exists(classification >= _))
          DOWNLOAD
        else
          VIEW
      else
        NONE
  }

  final val publicDateFields = Array(org.joda.time.DateTimeFieldType.year)
}

trait HasPermission extends PerSite {
  def permission : Permission.Value
  final def checkPermission(need : Permission.Value) : Boolean =
    Permission.check(permission, need)(site)
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
}

/** The possible types of data sensitivity according to the presence of identifying user data.
  * Must match the corresponding postgres "consent" type. */
object Classification extends PGEnum("classification") {
  val IDENTIFIED, EXCERPT, DEIDENTIFIED, ANALYSIS, PRODUCT, MATERIAL = Value
  def RESTRICTED = IDENTIFIED
  /** ANALYSIS and above are non-data and so unrestricted. */
  def UNRESTRICTED = ANALYSIS

  /** The most restricted data classification level that the current user may access under the given consent level.
    * Actual access to data will additionally depend on volume permissions not checked here. */
  def access(consent : Consent.Value, top : Boolean = false)(implicit site : Site) : Value = {
    val c = consent
    val a = site.access.group
    if (c >= Consent.PUBLIC ||
        c >= Consent.SHARED && a >= Permission.DOWNLOAD)
      IDENTIFIED
    else if (c == Consent.NONE && top)
      // "unassigned" excerpts are unrestricted, by assertion of PIs
      EXCERPT
    else
      DEIDENTIFIED
  }

  /** The most restricted data classification level that the current user may access under the given permission and consent level. */
  def download(p : Permission.Value, consent : Consent.Value, top : Boolean = false)(implicit site : Site) : Option[Value] = {
    if (p >= Permission.FULL || site.superuser)
      Some(Classification.values.min)
    else if (p >= Permission.DOWNLOAD)
      Some(Classification.access(consent, top))
    else if (p >= Permission.VIEW)
      Some(Classification.UNRESTRICTED)
    else
      None
  }
}
