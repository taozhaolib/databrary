package controllers

import scala.concurrent.Future
import play.api.Play
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
  private def publicResource(name : String, ext : String) =
    Play.resourceAsStream("/public/" + name + (if (Play.isDev) "." else ".min.") + ext)
    .fold(
      throw new RuntimeException("missing: " + name))(
      org.apache.commons.io.IOUtils.toString _)

  private def static[A : play.api.http.Writeable](name : String, content : => A) = {
    if (Play.isDev)
      Action(request => Ok(content))
    else {
      val data = content
      val etag = name + ":" + data.hashCode
      val now = new Timestamp
      val result = Ok(data)
	.withHeaders(
	  (ETAG, HTTP.quote(etag)),
	  (LAST_MODIFIED, HTTP.date(now)),
	  (CACHE_CONTROL, if (Play.isProd) "public, max-age=86400" else "no-cache"))
      Action { implicit request =>
	if (HTTP.notModified(etag, now)) NotModified
	else result
      }
    }
  }

  private val constantsJson = JsonObject(
      'messages -> json.Json.toJson(Messages.messages.get("default").map(
	/* hack to fix quoting (consider using https://github.com/SlexAxton/messageformat.js if things get more complicated) */
	_.mapValues(java.text.MessageFormat.format(_))))
    , 'permission -> json.Json.toJson(Permission.values.toSeq.map(_.toString))
    , 'consent -> json.Json.toJson(Consent.values.toSeq.map(_.toString))
    , 'classification -> json.Json.toJson(Classification.values.toSeq.map(_.toString))
    , 'category -> JsonRecord.map[RecordCategory](_.json)(RecordCategory.getAll)
    , 'format -> JsonRecord.map[AssetFormat](_.json)(AssetFormat.getAll)
    , 'party -> JsonObject('NOBODY->Party.NOBODY,'ROOT->Party.ROOT)
    , 'mode -> json.JsString(current.mode.toString)
    , 'version -> json.JsString(site.Site.version)
    ).js

  val constants = static("constants", constantsJson)
  val appJs = static("app.js",
    views.js.app(
      publicResource("javascripts/app", "js"),
      publicResource("templates/_all", "js"),
      constantsJson))

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
    async.when(request.isApi && request.headers.get("X-Requested-With").exists(_.equals("DatabraryClient")),
      request.headers.getAll("Analytics").foreachAsync(a =>
	scala.util.control.Exception.failAsValue[json.JsValue](classOf[com.fasterxml.jackson.core.JsonProcessingException])(json.JsUndefined("parse error"))(
	  json.Json.parse(a)) match {
	  case json.JsArray(l) => l.foreachAsync(analytic _)
	  case j => analytic(j)
	}))

  def void =
    SiteAction.Unlocked { implicit request =>
      Ok("")
    }
}
