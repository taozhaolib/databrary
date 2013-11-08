package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** A citation or reference to a publication or other external resource associated with a Volume.
  * This interface is temporary, quick and dirty, just to provide minimal functionality, and should not be expected to remain as is. */
final case class VolumeCitation(val volume : Volume, val head : String, val url : Option[String], val body : Option[String]) extends TableRow with InVolume {
  private def args = SQLTerms('volume -> volumeId, 'head -> head, 'url -> url, 'body -> body)
}

object VolumeCitation extends Table[VolumeCitation]("volume_citation") {
  private def make(volume : Volume)(head : String, url : Option[String], body : Option[String]) =
    new VolumeCitation(volume, head, url, body)
  private val columns = Columns(
      SelectColumn[String]("head")
    , SelectColumn[Option[String]]("url")
    , SelectColumn[Option[String]]("body")
    )
  private def volumeRow(vol : Volume) = columns map (make(vol) _)

  private[models] def getVolume(vol : Volume) : Future[Seq[VolumeCitation]] =
    volumeRow(vol).SELECT("WHERE volume = ? ORDER BY head").apply(vol.id).list

  private[models] def setVolume(vol : Volume, list : Seq[VolumeCitation]) : Future[Boolean] = {
    val l = list.map(_.ensuring(_.volume == vol).args)
    /* TODO: transaction */
    DELETE('volume -> vol.id).flatMap { _ =>
      Async.fold(l.map(
        INSERT(_).execute
      ), true)(_ && _)
    }
  }
}
