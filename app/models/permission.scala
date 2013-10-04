package models

import dbrary._
import util._

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

  /** The effective permission for data objects with the given attributes. */
  final def data(p : Value, consent : Consent.Value, classification : Classification.Value)(implicit site : Site) : Value = {
    if (p >= FULL)
      p
    else if (p >= DOWNLOAD && classification >= Classification.access(consent))
      DOWNLOAD
    else if (p >= VIEW && classification >= Classification.UNRESTRICTED)
      DOWNLOAD
    else if (p >= VIEW)
      VIEW
    else
      NONE
  }
}

/** The possible levels of participant consent governing [Classification.IDENTIFIED] data.
  * Must match the corresponding postgres "consent" type, except for the NONE value which represents NULL (missing) as this is a common possibility.
  * Should thus often be constructed as `consent.getOrElse(Consent.NONE)` and used as `util.maybe(consent, Consent.NONE)`. */
object Consent extends PGEnum("consent") {
  val NONE, PRIVATE, SHARED, EXCERPTS, PUBLIC = Value
}

/** The possible types of data sensitivity according to the presence of identifying user data.
  * Must match the corresponding postgres "consent" type. */
object Classification extends PGEnum("classification") {
  val IDENTIFIED, EXCERPT, DEIDENTIFIED, ANALYSIS, PRODUCT, MATERIAL = Value
  def RESTRICTED = IDENTIFIED
  /** ANALYSIS and above are non-data and so unrestricted. */
  def UNRESTRICTED = ANALYSIS

  /** The most restricetd data classification level that the current user may access under the given consent level.
    * Actual access to data will additionally depend on volume permissions not checked here. */
  def access(consent : Consent.Value)(implicit site : Site) : Value = {
    val c = consent
    val a = site.access
    if (// a >= Permission.ADMIN ||
        c >= Consent.PUBLIC ||
        c >= Consent.SHARED && a >= Permission.DOWNLOAD)
      IDENTIFIED
    else if (c >= Consent.EXCERPTS)
      EXCERPT
    else
      DEIDENTIFIED
  }
}
