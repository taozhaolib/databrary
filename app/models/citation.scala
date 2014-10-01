package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current
import java.net.URL
import macros._
import macros.async._
import dbrary._
import site._

/** A citation or reference to a publication or other external resource. */
final case class Citation(val head : String, val title : Option[String] = None, val url : Option[URL] = None, val authors : Option[IndexedSeq[String]] = None, val year : Option[Short] = None) {
  def orElse(other : Citation) =
    new Citation(
      Maybe(head) orElse other.head,
      title orElse other.title,
      url orElse other.url,
      authors orElse other.authors,
      year orElse other.year)

  def lookup(replace : Boolean = false) : Future[Citation] = 
    if (!replace && head.nonEmpty && title.nonEmpty && authors.nonEmpty && year.nonEmpty)
      async(this)
    else
      url.fold(async(this))(Citation.get(_).map(_.fold(this)(if (replace) _ orElse this else this orElse _)))

  def json = JsonObject.flatten(
    Some('head -> head),
    Some('title -> title),
    url.map('url -> _),
    authors.map('authors -> _),
    year.map('year -> _)
  )
}

object Citation {
  private val crossRef = "http://data.crossref.org/"
  private val bibliographyType = "text/x-bibliography"
  private val jsonType = "application/vnd.citationstyles.csl+json"

  import play.api.libs.json

  private def crossref(hdl : String, typ : String) =
    play.api.libs.ws.WS.url(crossRef + java.net.URLEncoder.encode(hdl))
    .withHeaders(("Accept", typ))
    .get.map { r =>
      if (r.status == 200 && r.header("Content-Type").equals(Some(Maybe(typ.indexOf(';')).fold(typ)(typ.substring(0, _)))))
        Some(r)
      else None
    }

  private def getHDLJson(hdl : String, style : String = "apa") : Future[Option[json.JsObject]] =
    crossref(hdl, jsonType)
    .flatMap(_.flatMap(_.json.asOpt[json.JsObject] /* XXX: json parse error? */)
      .filter(_.\("DOI").asOpt[String].exists(_.nonEmpty)).mapAsync { j =>
      crossref(hdl, bibliographyType + ";style=" + style).map(
        _.fold(j)(b => j + ("head" -> json.JsString(new String(
          /* empirically this is UTF-8, but does not say so: */
          b.body.getBytes(com.ning.http.util.AsyncHttpProviderUtils.DEFAULT_CHARSET)).trim))))
    })

  private def getURLJson(url : java.net.URL, style : String = "apa") : Future[Option[json.JsObject]] =
    if (url.getProtocol.equals("hdl") || url.getProtocol.equals("doi"))
      getHDLJson(url.getFile, style)
    else async(None)

  private def name(j : json.JsObject) : String =
    Seq("given", "non-dropping-particle", "family", "suffix")
    .flatMap(j.\(_).asOpt[String])
    .mkString(" ")

  def get(url : java.net.URL) : Future[Option[Citation]] =
    getURLJson(url).map(_.map { j =>
      new Citation(
        head = (j \ "head").asOpt[String].getOrElse(""),
        title = (j \ "title").asOpt[String],
        url = Some(url),
        authors = (j \ "author").asOpt[IndexedSeq[json.JsObject]].map(_.map(name)),
        year = (j \ "issued" \ "date-parts")(0)(0).asOpt[Short])
    })
}

object VolumeCitation extends Table[Citation]("volume_citation") {
  private val columns = Columns(
      SelectColumn[String]("head")
    , SelectColumn[Option[URL]]("url")
    , SelectColumn[Option[IndexedSeq[String]]]("authors")
    , SelectColumn[Option[Short]]("year")
    ).map { (head, url, authors, year) =>
      new Citation(head = head, url = url, authors = authors, year = year)
    }

  private[models] def get(vol : Volume) : Future[Option[Citation]] =
    columns.map(_.copy(title = Some(vol.name)))
    .SELECT("WHERE volume = ?")
    .apply(vol.id).singleOpt

  private[models] def set(vol : Volume, cite : Option[Citation]) : Future[Boolean] = {
    implicit val site = vol.site
    cite.fold {
      Audit.remove("volume_citation", SQLTerms('volume -> vol.id))
    } { cite =>
      Audit.changeOrAdd("volume_citation", SQLTerms('head -> cite.head, 'url -> cite.url, 'authors -> cite.authors, 'year -> cite.year), SQLTerms('volume -> vol.id))
    }.execute
  }
}
