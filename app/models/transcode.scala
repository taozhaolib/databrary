package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import site._

final class Transcode(assetId : Asset.Id, userId : Party.Id, start : Option[Timestamp], process : Option[Int], result : Option[String])

object Transcode extends Table[Transcode]("transcode") {
  def get(assetId : Asset.Id, process : Int) : Future[Option[Site]] =
    Party.row
    .leftJoin(Authorization.columns, "party.id = authorize_view.child AND authorize_view.parent = ?")
    .from(_ + " JOIN transcode ON party.id = user")
    .map { case (p, a) => new LocalAuth(Authorization.make(p)(a)) }
    .SELECT("WHERE asset = ? AND process = ?")
    .apply(assetId, process)
    .singleOpt
}
