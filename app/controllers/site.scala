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
import dbrary._
import site._
import models._

sealed trait SiteRequest[A] extends Request[A] with Site {
  val clientIP = Inet(remoteAddress)
  def withObj[O](obj : O) : RequestObject[O]#Site[A]
  val isApi = path.startsWith("/api/")
  def apiOptions : JsonOptions.Options = queryString
}

abstract class SiteException extends Exception with Results {
  def resultHtml(implicit request : SiteRequest[_]) : Future[SimpleResult]
  def resultApi : Future[SimpleResult]
  def result(implicit request : SiteRequest[_]) : Future[SimpleResult] =
    if (request.isApi) resultApi else resultHtml
}

abstract trait HtmlException extends SiteException {
  def resultApi = ???
  final override def result(implicit request : SiteRequest[_]) : Future[SimpleResult] =
    resultHtml
}

abstract trait ApiException extends SiteException {
  def resultHtml(implicit request : SiteRequest[_]) = ???
  final override def result(implicit request : SiteRequest[_]) : Future[SimpleResult] =
    resultApi
}

private[controllers] object NotFoundException extends SiteException {
  def resultHtml(implicit request : SiteRequest[_]) = macros.Async(NotFound(views.html.error.notFound(request)))
  def resultApi = macros.Async(NotFound)
}

private[controllers] object ForbiddenException extends SiteException {
  def resultHtml(implicit request : SiteRequest[_]) = macros.Async(Forbidden(views.html.error.forbidden(request)))
  def resultApi = macros.Async(Forbidden)
}

object ServiceUnavailableException extends SiteException {
  def resultHtml(implicit request : SiteRequest[_]) = macros.Async(ServiceUnavailable) // TODO page content
  def resultApi = macros.Async(ServiceUnavailable)
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
  def permission[O <: HasPermission](perm : Permission.Value = Permission.VIEW) = new ActionChecker[RequestObject[O]#Site] {
    protected def check[A](request : RequestObject[O]#Site[A]) =
      macros.Async(if (!request.obj.checkPermission(perm)) throw ForbiddenException)
  }
  def check[O <: HasPermission](get : SiteRequest[_] => Future[Option[O]], perm : Permission.Value = Permission.VIEW) =
    getter(get) ~> permission(perm)
  def check[O <: InVolume](v : models.Volume.Id, get : SiteRequest[_] => Future[Option[O]], perm : Permission.Value = Permission.VIEW) =
    getter(get(_).map(_.filter(_.volumeId === v))) ~> permission(perm)
}

object SiteAction extends ActionCreator[SiteRequest.Base] {
  def invokeBlock[A](request : Request[A], block : SiteRequest.Base[A] => Future[SimpleResult]) = {
    val now = new Timestamp
    macros.Async.flatMap(request.session.get("session"), models.SessionToken.get _).flatMap { session =>
      implicit val site = SiteRequest[A](request, session)
      if (session.exists(!_.valid))
        Async.foreach[SessionToken, Unit](session, _.remove).map { _ =>
          LoginController.needed("login.expired")
        }
      else
        /* we have to catch immediate exceptions, too */
        macros.Async.catching(classOf[SiteException])(block(site))
          .recoverWith {
            case e : SiteException => e.result
          }
          .map(_.withHeaders(HeaderNames.DATE -> HTTP.date(now)))
    }
  }

  object Auth extends ActionRefiner[SiteRequest,SiteRequest.Auth] {
    protected def refine[A](request : SiteRequest[A]) = macros.Async(request match {
      case request : SiteRequest.Auth[A] => Right(request)
      case _ => Left(LoginController.needed("login.noCookie")(request))
    })
  }

  val auth : ActionCreator[SiteRequest.Auth] = ~>(Auth)

  class AccessCheck[R[_] <: SiteRequest[_]](check : site.Access => Boolean) extends ActionHandler[R] {
    def handle[A](request : R[A]) = macros.Async {
      if (check(request.access)) None
      else Some(Results.Forbidden)
    }
  }

  case class Access[R[_] <: SiteRequest[_]](access : Permission.Value) extends AccessCheck[R](_.group >= access)
  case class RootAccess[R[_] <: SiteRequest[_]](permission : Permission.Value = Permission.ADMIN) extends AccessCheck[R](_.permission >= permission)

  def access(access : Permission.Value) = auth ~> Access[SiteRequest.Auth](access)
  def rootAccess(permission : Permission.Value = Permission.ADMIN) = auth ~> RootAccess[SiteRequest.Auth](permission)
}

private[controllers] abstract class FormException(form : Form[_]) extends SiteException {
  protected final implicit val jsonFormErrors : json.OWrites[Seq[FormError]] =
    json.OWrites[Seq[FormError]](errs =>
      json.JsObject(errs
        .groupBy(e => Maybe(e.key).orElse("error"))
        .mapValues(e => json.JsArray(
          e.map { case FormError(_, msg, args) => json.JsString(Messages(msg, args : _*)) }))
        .toSeq))
  def resultApi : Future[SimpleResult] =
    macros.Async(BadRequest(json.Json.toJson(form.errors)))
}

private[controllers] final class BadFormException[A](view : Form[A] => templates.HtmlFormat.Appendable)(form : Form[A]) extends FormException(form) {
  def resultHtml(implicit site : SiteRequest[_]) = macros.Async(BadRequest(view(form)))
}
private[controllers] final class ABadFormException[A](view : Form[A] => Future[templates.HtmlFormat.Appendable])(form : Form[A]) extends FormException(form) {
  def resultHtml(implicit site : SiteRequest[_]) = view(form).map(BadRequest(_))
}
private[controllers] final class ApiFormException(form : Form[_]) extends FormException(form) with ApiException

class SiteController extends Controller {
  protected def AOk[C : Writeable](c : C) : Future[SimpleResult] = macros.Async(Ok[C](c))
  protected def ABadRequest[C : Writeable](c : C) : Future[SimpleResult] = macros.Async(BadRequest[C](c))
  protected def ARedirect(c : Call) : Future[SimpleResult] = macros.Async(Redirect(c))
  protected def ANotFound(implicit request : SiteRequest[_]) : Future[SimpleResult] =
    NotFoundException.result
  protected def ABadForm[A](view : Form[A] => templates.HtmlFormat.Appendable, form : Form[A])(implicit request : SiteRequest[_]) : Future[SimpleResult] =
    new BadFormException(view)(form).result
  protected def AbadForm[A](view : Form[A] => Future[templates.HtmlFormat.Appendable], form : Form[A])(implicit request : SiteRequest[_]) : Future[SimpleResult] =
    new ABadFormException(view)(form).result
}

class ObjectController[O <: SiteObject] extends SiteController {
  type Request[A] = RequestObject[O]#Site[A]

  private[controllers] def result(o : O)(implicit request : SiteRequest[_]) : SimpleResult =
    if (request.isApi) Ok(o.json.js)
    else Redirect(o.pageURL)
}

object Site extends SiteController {
  assert(current.configuration.getString("application.secret").exists(_ != "databrary"),
    "Application is insecure. You must set application.secret appropriately (see README).")

  def start = VolumeHtml.search

  def test = Action { request =>
    Ok(request.queryString.toString)
  }

  def tinyUrl(prefix : String, path : String) = Action {
    MovedPermanently("/" + prefix + "/" + path)
  }

  def untrail(path : String) = Action {
    MovedPermanently("/" + path)
  }
}
