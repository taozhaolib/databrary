package object site {
  implicit def siteDB : Site.DB = Site.dbPool
}
