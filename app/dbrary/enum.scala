package dbrary

import anorm.{Column,ToStatement}

abstract class PGEnum(name : String) extends Enumeration {
  object PG extends PGType[Value](name, withName(_), _.toString)
  implicit val column = PG.column
  implicit val statement = PG.statement
}

object Permission extends PGEnum("permission") {
  val NONE, VIEW, DOWNLOAD, CONTRIBUTE, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  def EDIT = CONTRIBUTE
  def DATA = DOWNLOAD
  def OWN = ADMIN
}

object AuditAction extends PGEnum("audit_action") {
  val login, logout, add, change, remove, download = Value
}

object Consent extends PGEnum("consent") {
  val PUBLIC, IDENTIFIED, EXCERPTS, SHARED, PRIVATE = Value
}
