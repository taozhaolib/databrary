package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

/** A funding source associated with a Volume, where the agency is represented as a party.
  * This interface is temporary, quick and dirty, just to provide minimal functionality, and should not be expected to remain as is. */
final case class VolumeFunding(val volume : Volume, val funder : Party, val grant : Option[String]) extends TableRow with InVolume {
  def funderId = funder.id
  private def args = SQLTerms('volume -> volumeId, 'funder -> funderId, 'grant -> grant)

  def json = JsonObject.flatten(
    Some('volume -> volume.json),
    Some('party -> funder.json),
    grant.map('grant -> _)
  )
}

object VolumeFunding extends Table[VolumeFunding]("volume_funding") {
  private val columns = Columns(
      SelectColumn[Option[String]]("grant")
    ).map { (grant) =>
      (volume : Volume, funder : Party) => new VolumeFunding(volume, funder, grant)
    }
  private def volumeRow(vol : Volume) =
    columns.join(Party.row, "volume_funding.funder = party.id") map {
      case (fund, party) => fund(vol, party)
    }

  private[models] def getVolume(vol : Volume) : Future[Seq[VolumeFunding]] =
    columns.join(Party.row, "volume_funding.funder = party.id")
      .map { case (ing, er) => ing(vol, er) }
      .SELECT("WHERE volume = ?").apply(vol.id).list

  private[models] def getFunder(party : Party)(implicit site : Site) : Future[Seq[VolumeFunding]] =
    columns.join(Volume.row, "volume_funding.volume = volume.id")
      .map { case (fund, vol) => fund(vol, party) }
      .SELECT("WHERE funder = ? AND", Volume.condition)
      .apply(party.id +: Volume.conditionArgs).list

  private[models] def setVolume(vol : Volume, list : Seq[VolumeFunding]) : Future[Boolean] = {
    val l = list.map(_.ensuring(_.volume == vol).args)
    /* TODO: transaction */
    DELETE('volume -> vol.id).flatMap { _ =>
      Async.map[SQLTerms, Boolean, Seq[Boolean]](l,
        INSERT(_).execute
      ).map(_.forall(identity))
    }
  }
}
