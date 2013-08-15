import play.api._
import play.api.mvc._
import play.api.mvc.Results._

object Global extends GlobalSettings {
  override def onBadRequest(request: RequestHeader, error: String) = {
    if(play.api.Play.isDev(play.api.Play.current)){
      super.onBadRequest(request, error)
    }else{
      BadRequest(views.html.error.on400(request, error))
    }
  }

  override def onError(request: RequestHeader, throwable: Throwable) = {
    if(play.api.Play.isDev(play.api.Play.current)){
      super.onError(request, throwable)
    }else{
      InternalServerError(views.html.error.on500(throwable))
    }
  }

  override def onHandlerNotFound(request: RequestHeader) = {
    if(play.api.Play.isDev(play.api.Play.current)){
      super.onHandlerNotFound(request)
    }else{
      NotFound(views.html.error.on404(request))
    }
  }
}