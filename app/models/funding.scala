package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import site._

/** A funding source associated with a Volume, where the agency is represented as a party.
  * This interface is temporary, quick and dirty, just to provide minimal functionality, and should not be expected to remain as is. */
final case class VolumeFunding(val volume : Volume, val funder : Party, val grant : Option[String]) extends TableRow with InVolume {
  def funderId = funder.id
  private def args = SQLTerms('volume -> volumeId, 'funder -> funderId, 'grant -> grant)
}

object VolumeFunding extends Table[VolumeFunding]("volume_funding") {
  private def make(volume : Volume, funder : Party)(grant : Option[String]) =
    new VolumeFunding(volume, funder, grant)
  private val columns = Columns(
      SelectColumn[Option[String]]("grant")
    )
  private def volumeRow(vol : Volume) = columns.join(Party.row, "volume_funding.funder = party.id") map {
      case (fund, party) => make(vol, party)(fund)
    }

  private[models] def getVolume(vol : Volume) : Future[Seq[VolumeFunding]] =
    columns.join(Party.row, "volume_funding.funder = party.id")
      .map { case (ing, er) => make(vol, er)(ing) }
      .SELECT("WHERE volume = ?").apply(vol.id).list

  private[models] def getFunder(party : Party)(implicit site : Site) : Future[Seq[VolumeFunding]] =
    columns.join(Volume.row, "volume_funding.volume = volume.id")
      .map { case (fund, vol) => make(vol, party)(fund) }
      .SELECT("WHERE funder = ? AND", Volume.condition)
      .apply(party.id +: Volume.conditionArgs).list

  def setVolume(vol : Volume, list : Seq[VolumeFunding]) : Unit = {
    val l = list.map(_.ensuring(_.volume == vol).args)
    /* TODO: transaction */
    SQL("DELETE FROM volume_funding WHERE volume = ?").apply(vol.id).flatMap { _ =>
      Future.sequence(l map { a =>
        SQL("INSERT INTO volume_funding " + a.insert).apply(a).execute
      }).map(_.forall(identity))
    }
  }
}
