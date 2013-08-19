package models

import dbrary._

object Permission extends PGEnum("permission") {
  val NONE, VIEW, DOWNLOAD, CONTRIBUTE, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  def OWN = ADMIN
  def EDIT = CONTRIBUTE
  def DATA = DOWNLOAD
  def COMMENT = VIEW
}

object Consent extends PGEnum("consent") {
  val NONE, PRIVATE, SHARED, EXCERPTS, PUBLIC = Value
}
