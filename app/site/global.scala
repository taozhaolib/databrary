package site

import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.Play.current
import scala.concurrent.Future

object Global extends GlobalSettings {
  override def onBadRequest(request: RequestHeader, error: String) = {
    if (Play.isDev)
      super.onBadRequest(request, error)
    else
      Future.successful(BadRequest(views.html.error.on400(request, error)))
  }

  override def onError(request: RequestHeader, throwable: Throwable) = {
    if (Play.isDev)
      super.onError(request, throwable)
    else
      Future.successful(InternalServerError(views.html.error.on500(request, throwable)))
  }

  override def onHandlerNotFound(request: RequestHeader) = {
    if (Play.isDev)
      super.onHandlerNotFound(request)
    else
      Future.successful(NotFound(views.html.error.on404(request)))
  }
}
