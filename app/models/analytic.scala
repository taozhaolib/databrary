package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import dbrary._
import site._

final class Analytic(when : Timestamp, who : Party.Id, ip : Inet, action : Audit.Action.Value, route : String, data : JsValue)
  extends AuditBase(when, who, ip, action)

object Analytic extends Table[Analytic]("audit.analytic") {
  def add(action : Audit.Action.Value, route : String, data : JsValue)(implicit site : Site) : Future[Boolean] =
    INSERT(Audit.aargs(action) ++ SQLTerms('route -> route, 'data -> data)).execute
}
