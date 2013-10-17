package models

import anorm._
import dbrary._
import dbrary.Anorm._
import site._

/** A funding source associated with a Volume, where the agency is represented as a party.
  * This interface is temporary, quick and dirty, just to provide minimal functionality, and should not be expected to remain as is. */
final case class VolumeFunding(val volume : Volume, val funderId : Party.Id, val grant : Option[String]) extends TableRow with InVolume {
  private def args = SQLArgs('volume -> volumeId, 'funder -> funderId, 'grant -> grant)
  private val _funder = CachedVal[Party,Site](Party.get(funderId)(_).get)
  def funder(implicit site : Site) : Party = _funder
}

object VolumeFunding extends Table[VolumeFunding]("volume_funding") {
  private def make(volume : Volume)(funderId : Party.Id, grant : Option[String]) =
    new VolumeFunding(volume, funderId, grant)
  private val columns = Columns[
    Party.Id, Option[String]](
    'funder,  'grant)
  private[models] val row = columns.join(Volume.row, "volume_funding.volume = volume.id").
    map { case (fund ~ vol) => (make(vol) _).tupled(fund) }
  private def volumeRow(vol : Volume) = columns.join(Party.row, "volume_funding.funder = party.id").
    map { case (fund ~ party) =>
      val f = (make(vol) _).tupled(fund)
      f._funder() = party
      f
    }

  private[models] def getVolume(vol : Volume)(implicit db : Site.DB) : Seq[VolumeFunding] =
    volumeRow(vol).SQL("WHERE volume = {vol}").on('vol -> vol.id).list

  private[models] def getFunder(party : Party)(implicit site : Site) : Seq[VolumeFunding] =
    row.map { f =>
        f._funder() = party
        f
      }.SQL("WHERE funder = {party} AND", Volume.condition).
      on(Volume.conditionArgs('party -> party.id) : _*).list

  def setVolume(vol : Volume, list : Seq[VolumeFunding])(implicit db : Site.DB) : Unit = {
    SQL("DELETE FROM volume_funding WHERE volume = {vol}").on('vol -> vol.id).execute
    if (list.isEmpty)
      return
    val l = list.map(_.ensuring(_.volume == vol).args)
    SQL("INSERT INTO volume_funding " + l.head.insert).addBatchList(l).execute
  }
}
