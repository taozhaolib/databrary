package site

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._

object Cite {
  private val crossRef = "http://data.crossref.org/"
  private val bibliographyType = "text/x-bibliography"

  def getBibliography(hdl : String, style : String = "apa") : Future[Option[String]] =
    play.api.libs.ws.WS.url(crossRef + java.net.URLEncoder.encode(hdl))
    .withHeaders(("Accept", bibliographyType + ";style=" + style))
    .get.map { r =>
      if (r.status == 200 && r.header("Content-Type").equals(Some(bibliographyType)))
	Some(r.body)
      else None
    }

  def getBibliography(url : java.net.URL) : Future[Option[String]] =
    if (url.getProtocol.equals("hdl") || url.getProtocol.equals("doi"))
      getBibliography(url.getFile)
    else async(None)
}
