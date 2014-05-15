package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import macros.async._
import dbrary._
import site._

/** A citation or reference to a publication or other external resource associated with a Volume.
  * This interface is temporary, quick and dirty, just to provide minimal functionality, and should not be expected to remain as is. */
final case class VolumeCitation(val volume : Volume, val head : String, val url : Option[java.net.URL], val body : Option[String]) extends TableRow with InVolume {
  private[models] def sqlKey = SQLTerms('volume -> volumeId)
  private def args = SQLTerms('volume -> volumeId, 'head -> head, 'url -> url.map(_.toString), 'body -> body)

  def json = JsonObject.flatten(
    Some('head -> head),
    url.map(u => ('url, u.toString)),
    body.map('body -> _)
  )
}

object VolumeCitation extends Table[VolumeCitation]("volume_citation") {
  private val columns = Columns(
      SelectColumn[String]("head")
    , SelectColumn[Option[String]]("url")
    , SelectColumn[Option[String]]("body")
    ).map { (head, url, body) =>
      (vol : Volume) => new VolumeCitation(vol, head, url.flatMap(dbrary.url.parse _), body)
    }
  private def volumeRow(vol : Volume) = columns.map(_(vol))

  private[models] def getVolume(vol : Volume) : Future[Seq[VolumeCitation]] =
    volumeRow(vol).SELECT("WHERE volume = ? ORDER BY head").apply(vol.id).list

  private[models] def setVolume(vol : Volume, list : Seq[VolumeCitation]) : Future[Boolean] = {
    val l = list.map(_.ensuring(_.volume == vol).args)
    val dbc = implicitly[Site.DB]
    val exc = implicitly[ExecutionContext]
    dbc.inTransaction { dbc =>
      DELETE('volume -> vol.id)(dbc, exc).flatMap { _ =>
	l.mapAsync(
	  INSERT(_)(dbc, exc).execute
	).map(_.forall(identity))
      }
    }
  }
}
