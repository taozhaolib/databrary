package controllers

import play.api.mvc._
import macros._
import dbrary._
import site._
import models._

object SiteApi extends SiteController {
  private final val startTime = new Timestamp

  private final val constantsJson = JsonObject(
      'permission -> JsonRecord.map[Permission.Value](c => JsonRecord(c.id
	, 'name -> c.toString
	))(Permission.values.toSeq)
    , 'consent -> JsonRecord.map[Consent.Value](c => JsonRecord.flatten(c.id
	, Some('name -> c.toString)
	, Consent.description(c).map('message -> _)
	))(Consent.values.toSeq)
    , 'classification -> JsonRecord.map[Classification.Value](c => JsonRecord(c.id
	, 'name -> c.toString
	))(Classification.values.toSeq)
    // to be consistent with volume."categories":
    , 'categories -> JsonRecord.map[RecordCategory](_.json)(RecordCategory.getAll)
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
