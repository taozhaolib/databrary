package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import dbrary.SQL._
import site._

object Activity {
  def volumes(limit : Int = 8)(implicit site : Site) : Future[Seq[(Timestamp, Volume)]] =
    Columns(SelectColumn[Timestamp]("a", "audit_time"))(FromTable("audit.volume_access AS a"))
    .join(Volume.row on "a.volume = volume.id")
    .SELECT(sql"WHERE a.audit_action = 'add' AND a.party = 0 AND a.children > 'NONE' AND "
      + Volume.condition ++ sql" ORDER BY a.audit_time DESC LIMIT $limit")
    .list

  def authorizations(limit : Int = 8)(implicit site : Site) : Future[Seq[(Timestamp, Party)]] =
    Columns(SelectColumn[Timestamp]("a", "audit_time"))(FromTable("audit.authorize AS a"))
    .join(Party.row on "a.child = party.id")
    .SELECT(sql"JOIN authorize_view ON a.parent = authorize_view.child AND authorize_view.parent = 0 WHERE a.audit_action IN ('add','change') AND a.site >= 'EDIT' AND authorize_view.site > 'EDIT' ORDER BY a.audit_time DESC LIMIT $limit")
    .list
}
