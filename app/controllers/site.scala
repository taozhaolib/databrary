package controllers
import scala.language.higherKinds

import scala.concurrent.Future
import play.api._
import          Play.current
import          http._
import          mvc._
import          data._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import macros._
import macros.async._
import dbrary._
import site._
import models._

sealed trait SiteRequest[A] extends Request[A] with Site {
  val clientIP = Inet(remoteAddress)
  def withObj[O](obj : O) : RequestObject[O]#Site[A]
  val isApi = path.startsWith("/api/")
  def apiOptions : JsonOptions.Options = queryString
}

object SiteRequest {
  sealed abstract class Base[A](request : Request[A]) extends WrappedRequest[A](request) with SiteRequest[A]
  sealed class Anon[A](request : Request[A]) extends Base[A](request) with AnonSite {
    def withObj[O](obj : O) : RequestObject[O]#Anon[A] = {
      object ro extends RequestObject[O]
      new ro.Anon(request, obj)
    }
  }
  sealed class Auth[A](request : Request[A], val token : SessionToken) extends Base[A](request) with AuthSite {
    val superuser = token.superuser(request)
    def withObj[O](obj : O) : RequestObject[O]#Auth[A] = {
      object ro extends RequestObject[O]
      new ro.Auth(request, token, obj)
    }
  }
  def apply[A](request : Request[A], session : Option[SessionToken]) : Base[A] =
    session.filter(_.valid).fold[Base[A]](
      new Anon[A](request))(
      new Auth[A](request, _))
}

abstract class SiteException extends Exception with Results {
  def resultHtml(implicit request : SiteRequest[_]) : Future[Result]
  def resultApi : Future[Result]
  def result(implicit request : SiteRequest[_]) : Future[Result] =
    if (request.isApi) resultApi else resultHtml

  class ActionCheck[R[_] <: SiteRequest[_]](check : R[_] => Future[Boolean]) extends ActionFilter[R] {
    def filter[A](request : R[A]) =
      check(request).flatMap {
        case true => async(None)
        case false => result(request).map(Some(_))
      }
  }
}

object SiteException extends ActionFunction[SiteRequest.Base, SiteRequest.Base] {
  def invokeBlock[A](request : SiteRequest.Base[A], block : SiteRequest.Base[A] => Future[Result]) =
    /* we have to catch immediate exceptions, too */
    async.catching(classOf[SiteException])(block(request))
      .recoverWith {
        case e : SiteException => e.result(request)
      }
}

abstract trait HtmlException extends SiteException {
  def resultApi = ???
  final override def result(implicit request : SiteRequest[_]) : Future[Result] =
    resultHtml
}

abstract trait ApiException extends SiteException {
  def resultHtml(implicit request : SiteRequest[_]) = ???
  final override def result(implicit request : SiteRequest[_]) : Future[Result] =
    resultApi
}

private[controllers] object NotFoundException extends SiteException {
  def resultHtml(implicit request : SiteRequest[_]) = async(NotFound(views.html.error.notFound(request)))
  def resultApi = async(NotFound)
}

private[controllers] object ForbiddenException extends SiteException {
  def resultHtml(implicit request : SiteRequest[_]) = async(Forbidden(views.html.error.forbidden(request)))
  def resultApi = async(Forbidden)
}

object ServiceUnavailableException extends SiteException {
  def resultHtml(implicit request : SiteRequest[_]) = async(ServiceUnavailable) // TODO page content
  def resultApi = async(ServiceUnavailable)
}

trait RequestObject[+O] {
  sealed trait Base[A] extends Request[A] {
    val obj : O
  }
  sealed trait Site[A] extends SiteRequest[A] with Base[A]
  final class Anon[A](request : Request[A], val obj : O) extends SiteRequest.Anon[A](request) with Site[A]
  final class Auth[A](request : Request[A], token : SessionToken, val obj : O)
    extends SiteRequest.Auth[A](request, token) with Site[A]
}
object RequestObject {
  type Site[A] = RequestObject[SiteObject]#Site[A]

  def getter[O](get : SiteRequest[_] => Future[Option[O]]) = new ActionTransformer[SiteRequest.Base,RequestObject[O]#Site] {
    protected def transform[A](request : SiteRequest.Base[A]) =
      get(request).map(_.fold[RequestObject[O]#Site[A]](
        throw NotFoundException)(
        o => request.withObj(o)))
  }
  def permission[O <: HasPermission](perm : Permission.Value = Permission.VIEW) =
    new ForbiddenException.ActionCheck[RequestObject[O]#Site](request => async(request.obj.checkPermission(perm)))
  def check[O <: HasPermission](get : SiteRequest[_] => Future[Option[O]], perm : Permission.Value = Permission.VIEW) =
    getter(get) andThen permission(perm)

  def cast(request : SiteRequest[_]) : Option[Any] =
    request match {
      case ro : RequestObject[_]#Site[_] => Some(ro.obj)
      case _ => None
    }
}

object SiteAction extends ActionBuilder[SiteRequest.Base] {
  def invokeBlock[A](request : Request[A], block : SiteRequest.Base[A] => Future[Result]) = {
    val now = System.currentTimeMillis
    request.session.get("session").flatMapAsync(models.SessionToken.get _).flatMap { session =>
      val site = SiteRequest[A](request, session)
      AngularController.analytics(site).flatMap(_ =>
      SiteException.invokeBlock(site,
        if (session.exists(!_.valid)) { request : SiteRequest.Base[A] =>
          session.foreachAsync(_.remove).map { _ =>
            LoginController.needed("login.expired")(request)
          }
        } else block)
      .map { res =>
        _root_.site.Site.accessLog.log(now, request, res, Some(site.identity.id.toString))
        (if (site.isApi && !res.header.headers.contains(HeaderNames.CACHE_CONTROL)) res.withHeaders(
          HeaderNames.CACHE_CONTROL -> "no-cache") else res)
        .withHeaders(
          HeaderNames.DATE -> HTTP.date(new Timestamp(now)),
          HeaderNames.SERVER -> _root_.site.Site.appVersion)
      })
    }
  }

  object Auth extends ActionRefiner[SiteRequest,SiteRequest.Auth] {
    protected def refine[A](request : SiteRequest[A]) = macros.async(request match {
      case request : SiteRequest.Auth[A] => Right(request)
      case _ => Left(LoginController.needed("login.noCookie")(request))
    })
  }

  val auth : ActionBuilder[SiteRequest.Auth] = andThen(Auth)

  class AccessCheck[R[_] <: SiteRequest[_]](check : site.Access => Boolean)
    extends ForbiddenException.ActionCheck[R](request => macros.async(check(request.access)))

  case class Access[R[_] <: SiteRequest[_]](access : Permission.Value) extends AccessCheck[R](_.site >= access)
  case class RootMember[R[_] <: SiteRequest[_]](access : Permission.Value) extends AccessCheck[R](_.member >= access)
  case class RootAccess[R[_] <: SiteRequest[_]](permission : Permission.Value = Permission.ADMIN) extends AccessCheck[R](_.permission >= permission)

  def access(access : Permission.Value) = auth andThen Access[SiteRequest.Auth](access)
  def rootMember(permission : Permission.Value = Permission.ADMIN) = auth andThen RootMember[SiteRequest.Auth](permission)
  def rootAccess(permission : Permission.Value = Permission.ADMIN) = auth andThen RootAccess[SiteRequest.Auth](permission)

  object JS extends ActionBuilder[Request] {
    def invokeBlock[A](request : Request[A], block : Request[A] => Future[Result]) : Future[Result] =
      if (request.method == "GET" && !request.path.startsWith("/api/") && AngularController.jsEnabled(request))
        AngularController.page(request.asInstanceOf[Request[AnyContent]])
      else
        block(request)
  }
  val js = JS andThen SiteAction
}

private[controllers] abstract class FormException(form : Form[_], status : Results.Status = Results.BadRequest)
  extends SiteException {
  def resultApi : Future[Result] =
    async(status(form.errorsAsJson))
}

private[controllers] final class ApiFormException(form : Form[_], status : Results.Status = Results.BadRequest)
  extends FormException(form, status) with ApiException

class SiteController extends Controller {
  protected def AOk[C : Writeable](c : C) : Future[Result] = async(Ok[C](c))
  protected def ABadRequest[C : Writeable](c : C) : Future[Result] = async(BadRequest[C](c))
  protected def ARedirect(c : Call) : Future[Result] = async(Redirect(c))
  protected def ANotFound(implicit request : SiteRequest[_]) : Future[Result] =
    NotFoundException.result
}

class ObjectController[O <: SiteObject] extends SiteController {
  final type Request[A] = RequestObject[O]#Site[A]

  private[controllers] final def result(o : O)(implicit request : SiteRequest[_]) : Result =
    if (request.isApi) Ok(o.json)
    else Redirect(o.pageURL)
}

trait ApiController extends SiteController
trait HtmlController extends SiteController

object Site extends SiteController {
  assert(current.configuration.getString("application.secret").exists(_ != "databrary"),
    "Application is insecure. You must set application.secret appropriately (see README).")

  def tinyUrl(prefix : String, path : String) = Action {
    MovedPermanently("/" + prefix + "/" + path)
  }

  def moved(path : String) = Action {
    MovedPermanently("/" + path)
  }

  def favicon =
    Assets.at("/public/icons", "favicon.ico")

  def robots = Action {
    Ok(views.txt.robots())
  }
}

object SiteHtml extends HtmlController {
  assert(current.configuration.getString("application.secret").exists(_ != "databrary"),
    "Application is insecure. You must set application.secret appropriately (see README).")

  def start(js : Option[Boolean]) =
    SiteAction.js.async { implicit request =>
      VolumeHtml.viewSearch(request)
    }

  def search(js : Option[Boolean]) =
    VolumeHtml.search(js)

  def permissions() = Action { implicit request =>
    Ok(views.html.permissions())
  }
}

object SiteApi extends ApiController {
  def activity = SiteAction.async { implicit request =>
    for {
      va <- Activity.volumes(8)
      vl <- va.mapAsync { case (t, v) =>
        v.json(Map("access" -> Seq("ADMIN"))).map(j => (t, JsonField("volume", j)))
      }
      aa <- Activity.authorizations(8)
      al = aa.map { case (t, p) => (t, JsonField("party", p.json)) }
      l = (vl ++ al).sortWith((a, b) => a._1.isAfter(b._1)).take(12)
    } yield (Ok(JsonArray.map[(Timestamp, JsonField), JsonObject] { case (t, j) => JsonObject('time -> t, j) } (l)))
  }
}
