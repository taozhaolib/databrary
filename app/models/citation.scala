package models

import dbrary._
import site._

/** A citation or reference to a publication or other external resource associated with a Volume.
  * This interface is temporary, quick and dirty, just to provide minimal functionality, and should not be expected to remain as is. */
final case class VolumeCitation(val volume : Volume, val head : String, val url : Option[String], val body : Option[String]) extends TableRow with InVolume {
  private def args = SQLArgs('volume -> volumeId, 'head -> head, 'url -> url, 'body -> body)
}

object VolumeCitation extends Table[VolumeCitation]("volume_citation") {
  private def make(volume : Volume)(head : String, url : Option[String], body : Option[String]) =
    new VolumeCitation(volume, head, url, body)
  private val columns = Columns[
    String, Option[String], Option[String]](
    'head,  'url,           'body)
  private[models] val row = columns.join(Volume.row, "volume_citation.volume = volume.id") map {
    case (cite ~ vol) => (make(vol) _).tupled(cite)
  }
  private def volumeRow(vol : Volume) = columns map (make(vol) _)

  private[models] def getVolume(vol : Volume)(implicit db : Site.DB) : Seq[VolumeCitation] =
    volumeRow(vol).SQL("WHERE volume = {vol} ORDER BY head").on('vol -> vol.id).list

  def setVolume(vol : Volume, list : Seq[VolumeCitation])(implicit db : Site.DB) : Unit = {
    SQL("DELETE FROM volume_citation WHERE volume = {vol}").on('vol -> vol.id).execute
    if (list.isEmpty)
      return
    val l = list.map(_.ensuring(_.volume == vol).args)
    SQL("INSERT INTO volume_citation " + l.head.insert).addBatchList(l).execute
  }
}
