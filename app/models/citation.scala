package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current
import java.net.URL
import macros._
import macros.async._
import dbrary._
import dbrary.SQL._
import site._

sealed abstract class ExternalReference(head : String, url : Option[URL]) {
  def json = JsonObject.flatten(
    Some('head -> head),
    url.map('url -> _)
  )
}

/** A reference to an external resource. */
final class ExternalLink(val head : String, val url : URL)
  extends ExternalReference(head, Some(url))

/** A citation or reference to a publication or other external resource. */
final case class Citation(val head : String, val title : Option[String] = None, val url : Option[URL] = None, val year : Option[Short] = None)
  extends ExternalReference(head, url) {
  def orElse(other : Citation) =
    new Citation(
      Maybe(head) orElse other.head,
      title orElse other.title,
      url orElse other.url,
      year orElse other.year)

  def lookup(replace : Boolean = false) : Future[Citation] =
    if (!replace && head.nonEmpty && title.nonEmpty && year.nonEmpty)
      async(this)
    else
      url.fold(async(this))(Citation.get(_).map(_.fold(this)(if (replace) _ orElse this else this orElse _)))

  override def json = super.json ++ JsonObject.flatten(
    title.map('title -> _),
    year.map('year -> _)
  )
}

object Citation {
  private val crossRef = "http://data.crossref.org/"
  private val bibliographyType = "text/x-bibliography"
  private val jsonType = "application/vnd.citationstyles.csl+json"

  import play.api.libs.json

  private def crossref(hdl : String, typ : String) =
    play.api.libs.ws.WS.url(crossRef + java.net.URLEncoder.encode(hdl, "utf-8"))
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

  def get(url : java.net.URL) : Future[Option[Citation]] =
    getURLJson(url).map(_.map { j =>
      new Citation(
        head = (j \ "head").asOpt[String].getOrElse(""),
        title = (j \ "title").asOpt[String],
        url = Some(url),
        year = (j \ "issued" \ "date-parts")(0)(0).asOpt[Short])
    })
}

object VolumeLink extends Table[ExternalLink]("volume_link") {
  private val columns = Columns(
      SelectColumn[String]("head")
    , SelectColumn[URL]("url")
    ).map { (head, url) =>
      new ExternalLink(head = head, url = url)
    }

  def get(vol : Volume) : Future[Seq[ExternalLink]] =
    columns
    .SELECT(sql"WHERE volume = ${vol.id}")
    .list

  def set(vol : Volume, refs : Seq[ExternalLink]) : Future[Boolean] = {
    implicit val site = vol.site
    val i = SQLTerms('volume -> vol.id)
    implicitly[Site.DB].inTransaction { implicit siteDB => for {
      _ <- Audit.remove(table, i).execute
      _ <- refs.foreachAsync { r =>
        Audit.add(table, i ++ SQLTerms('head -> r.head, 'url -> r.url)).execute
      }
    } yield (true) }
    .recover {
      case SQLDuplicateKeyException() => false
    }
  }
}

object VolumeCitation extends Table[Citation]("volume_citation") {
  private val columns = Columns(
      SelectColumn[String]("head")
    , SelectColumn[Option[URL]]("url")
    , SelectColumn[Option[Short]]("year")
    ).map { (head, url, year) =>
      new Citation(head = head, url = url, year = year)
    }

  private[models] def get(vol : Volume) : Future[Option[Citation]] =
    columns.map(_.copy(title = Some(vol.name)))
    .SELECT(sql"WHERE volume = ${vol.id}")
    .singleOpt

  private[models] def set(vol : Volume, cite : Option[Citation]) : Future[Boolean] = {
    implicit val site = vol.site
    cite.fold {
      Audit.remove(table, SQLTerms('volume -> vol.id))
    } { cite =>
      Audit.changeOrAdd(table, SQLTerms('head -> cite.head, 'url -> cite.url, 'year -> cite.year), SQLTerms('volume -> vol.id))
    }.execute
  }
}
