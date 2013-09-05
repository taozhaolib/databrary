package object controllers {
  implicit def siteDB(implicit site : util.Site) : util.Site.DB = site.db
}
