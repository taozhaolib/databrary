package controllers

import play.api.mvc._
import macros._
import dbrary._
import site._
import models._

object SiteApi extends SiteController {
  private final val startTime = new Timestamp

  private final val constantsJson = JsonObject(
      'permision -> Permission.values.view.map(_.toString)
    , 'consent -> Consent.values.view.map(c =>
	JsonObject.flatten(
	  Some('name -> c.toString)
	, Consent.description(c).map('message -> _)
	)
      )
    , 'classification -> Classification.values.view.map(_.toString)
    , 'record_category -> JsonRecord.map[RecordCategory](_.json)(RecordCategory.getAll)
    ).js
  private final val constantsETag = "constants:" + constantsJson.hashCode
  private final val constantsResult = Ok(constantsJson)
    .withHeaders(
      LAST_MODIFIED -> HTTP.date(startTime)
    , ETAG -> HTTP.quote(constantsETag)
    , CACHE_CONTROL -> "max-age=86400"
    )
  def constants = Action { implicit request =>
    if (HTTP.notModified(constantsETag, startTime)) NotModified
    else constantsResult
  }
}
