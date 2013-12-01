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
import macros._
import dbrary._
import site._
import models._

sealed trait SiteRequest[A] extends Request[A] with Site {
  def clientIP = Inet(remoteAddress)
  def withObj[O](obj : O) : RequestObject[O]#Site[A]
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
    val account = token.account
    val access = token.access
    lazy val superuser = token.superuser(request)
    def withObj[O](obj : O) : RequestObject[O]#Auth[A] = {
      object ro extends RequestObject[O]
      new ro.Auth(request, token, obj)
    }
  }
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
  def getter[O](get : SiteRequest[_] => Future[Option[O]]) = new ActionRefiner[SiteRequest.Base,RequestObject[O]#Site] {
    protected def refine[A](request : SiteRequest.Base[A]) =
      get(request).map(_.fold[Either[SimpleResult,RequestObject[O]#Site[A]]](
        Left(Results.NotFound))(
        o => Right(request.withObj(o))))
  }
  def permission[O <: HasPermission](perm : Permission.Value = Permission.VIEW) = new ActionHandler[RequestObject[O]#Site] {
    protected def handle[A](request : RequestObject[O]#Site[A]) =
      Future.successful(if (request.obj.checkPermission(perm)) None else Some(Results.Forbidden))
  }
  def check[O <: HasPermission](get : SiteRequest[_] => Future[Option[O]], perm : Permission.Value = Permission.VIEW) =
    getter(get) ~> permission(perm)
  def check[O <: InVolume](v : models.Volume.Id, get : SiteRequest[_] => Future[Option[O]], perm : Permission.Value = Permission.VIEW) =
    getter(get(_).map(_.filter(_.volumeId == v))) ~> permission(perm)
}

object SiteAction extends ActionCreator[SiteRequest.Base] {
  def invokeBlock[A](request : Request[A], block : SiteRequest.Base[A] => Future[SimpleResult]) =
    macros.Async.flatMap(request.session.get("session"), models.SessionToken.get _).flatMap {
      case None =>
        block(new SiteRequest.Anon[A](request))
      case Some(session) if !session.valid =>
        implicit val site = new SiteRequest.Anon[A](request)
        /* XXX should this be some kind of global notification? */
        Async(Results.Forbidden(Login.viewLogin(Messages("login.expired"))).withNewSession)
      case Some(session) =>
        block(new SiteRequest.Auth[A](request, session))
    }

  object Auth extends ActionRefiner[SiteRequest,SiteRequest.Auth] {
    protected def refine[A](request : SiteRequest[A]) = macros.Async(request match {
      case request : SiteRequest.Auth[A] => Right(request)
      case _ => Left(Login.needLogin(request))
    })
  }

  val auth : ActionCreator[SiteRequest.Auth] = ~>(Auth)

  case class Access[R[_] <: SiteRequest[_]](access : Permission.Value) extends ActionHandler[R] {
    def handle[A](request : R[A]) = macros.Async {
      if (request.access >= access) None
      else Some(Results.Forbidden)
    }
  }

  def access(access : Permission.Value) = auth ~> Access[SiteRequest.Auth](access)
}

class SiteController extends Controller {
  protected def isAjax[A](implicit request : Request[A]) =
    request.headers.get("X-Requested-With").fold(false)(_.equals("XMLHttpRequest"))
  protected def isJson[A](implicit request : Request[A]) =
    request.headers.get("Content-Type").fold(false)(_.contains("json"))

  protected def isSecure : Boolean =
    current.configuration.getString("application.secret").exists(_ != "databrary").
      ensuring(s => s, "Application is insecure. You must set application.secret appropriately (see README).")

  protected def AOk[C : Writeable](c : C) : Future[SimpleResult] = macros.Async(Ok[C](c))
  protected def ABadRequest[C : Writeable](c : C) : Future[SimpleResult] = macros.Async(BadRequest[C](c))
  protected def ARedirect(c : Call) : Future[SimpleResult] = macros.Async(Redirect(c))
  protected def ANotFound : Future[SimpleResult] = macros.Async(NotFound) // FIXME: blank page
  protected def AForbidden : Future[SimpleResult] = macros.Async(Forbidden) // FIXME: blank page
}

object Site extends SiteController {
  def start = Login.view

  def test = Action { request =>
    Ok("Ok")
  }

  def tinyUrl(path : String, prefix : String) = Action {
    prefix match {
      case "party" => MovedPermanently("/party/" + path)
      case "volume" => MovedPermanently("/volume/" + path)
    }
  }

  def untrail(path : String) = Action {
    MovedPermanently("/" + path)
  }
}
