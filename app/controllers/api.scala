package controllers

import scala.concurrent.Future
import play.api.Play.current
import play.api.i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import play.api.mvc._
import macros._
import macros.async._
import dbrary._
import site._
import models._

object SiteApi extends SiteController {
  private final val startTime = new Timestamp

  private final val constantsJson = JsonObject(
      'messages -> json.Json.toJson(Messages.messages.get("default").map(
	/* hack to fix quoting (consider using https://github.com/SlexAxton/messageformat.js if things get more complicated) */
	_.mapValues(java.text.MessageFormat.format(_))))
    , 'permission -> JsonRecord.map[Permission.Value](c => JsonRecord(c.id
	, 'name -> c.toString
	))(Permission.values.toSeq)
    , 'consent -> JsonRecord.map[Consent.Value](c => JsonRecord.flatten(c.id
	, Some('name -> c.toString)
	))(Consent.values.toSeq)
    , 'classification -> JsonRecord.map[Classification.Value](c => JsonRecord(c.id
	, 'name -> c.toString
	))(Classification.values.toSeq)
    , 'category -> JsonRecord.map[RecordCategory](_.json)(RecordCategory.getAll)
    , 'mode -> json.JsString(current.mode.toString)
    , 'version -> json.JsString(site.Site.version)
    ).js
  private final val constantsETag = "constants:" + constantsJson.hashCode
  private final val constantsResult = Ok(constantsJson)
    .withHeaders(
      (LAST_MODIFIED, HTTP.date(startTime))
    , (ETAG, HTTP.quote(constantsETag))
    , (CACHE_CONTROL, "max-age=86400")
    )
  def constants = Action { implicit request =>
    if (HTTP.notModified(constantsETag, startTime)) NotModified
    else constantsResult
  }

  private def analytic(data : json.JsValue)(implicit site : Site) : Future[Unit] = data match {
    case json.JsObject(f) =>
      var action : Option[Audit.Action.Value] = None
      var route : Option[String] = None
      val lb = f.genericBuilder[(String,json.JsValue)]
      f.foreach {
	case ("action", json.JsString(v)) => action = Audit.Action.withNameOpt(v)
	case ("route", json.JsString(v)) => route = Some(v)
	case kv => lb += kv
      }
      action.foreachAsync(action =>
	route.foreachAsync(route =>
	  Analytic.add(action, route, json.JsObject(lb.result))))
    case _ => async.void
  }

  def analytics(implicit request : SiteRequest[_]) : Future[Unit] =
    if (request.isApi && request.headers.get("X-Requested-With").exists(_.equals("DatabraryClient")))
      request.headers.getAll("Analytics").foreachAsync(a =>
	scala.util.control.Exception.failAsValue[json.JsValue](classOf[com.fasterxml.jackson.core.JsonProcessingException])(json.JsUndefined("parse error"))(
	  json.Json.parse(a)) match {
	  case json.JsArray(l) => l.foreachAsync(analytic _)
	  case j => analytic(j)
	})
    else
      async.void

  def void =
    SiteAction.Unlocked { implicit request =>
      Ok("")
    }
}
