package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import macros.async._
import dbrary._
import site._

final class Transcode(assetId : Asset.Id, ownerId : Party.Id, start : Option[Timestamp], process : Option[Int], result : Option[String])

object Transcode extends Table[Transcode]("transcode") {
  def get(assetId : Asset.Id, process : Int) : Future[Option[Asset]] =
    Party.row
    .leftJoin(Authorization.columns, "party.id = authorize_view.child AND authorize_view.parent = 0")
    .from(_ + " JOIN transcode ON party.id = owner")
    .map { case (p, a) => new LocalAuth(Authorization.make(p)(a)) }
    .SELECT("WHERE asset = ? AND process = ?")
    .apply(assetId, process)
    .singleOpt.flatMap(_.flatMapAsync(Asset.get(assetId)(_)))
}
