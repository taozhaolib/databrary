package ingest {
  class IngestException(message : String) extends java.lang.RuntimeException(message) {
    def target : Option[site.SitePage] = None
  }

  final case class PopulateException(message : String, override val target : Option[site.SitePage] = None) extends IngestException(message)
  object PopulateException {
    def apply(message : String, target : site.SitePage) : PopulateException = PopulateException(message, Some(target))
  }
}

package object ingest {
  private[ingest] def optString[A](v : Option[A]) : String = v.fold("")(_.toString)

  private[ingest] def check(b : Boolean, t : => Exception) : scala.concurrent.Future[Unit] =
    if (b) scala.concurrent.Future.successful(()) else scala.concurrent.Future.failed(t)
}
