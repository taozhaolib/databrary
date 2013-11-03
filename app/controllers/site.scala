package controllers
import scala.language.higherKinds

import play.api._
import          Play.current
import          http._
import          mvc._
import          data._
import          i18n.Messages
import scala.concurrent.{Future,ExecutionContext}
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
  sealed class Auth[A](request : Request[A], val identity : Account, val superuser : Boolean) extends Base[A](request) with AuthSite {
    def withObj[O](obj : O) : RequestObject[O]#Auth[A] = {
      object ro extends RequestObject[O]
      new ro.Auth(request, identity, superuser, obj)
    }
  }

  def apply[A](request : Request[A], identity : Option[Account], superuser : Boolean) : SiteRequest.Base[A] =
    identity.fold[SiteRequest.Base[A]](new Anon[A](request))(a => new Auth[A](request, a, superuser && a.access == Permission.ADMIN))
}

trait RequestObject[+O] {
  sealed trait Base[A] extends Request[A] {
    val obj : O
  }
  sealed trait Site[A] extends SiteRequest[A] with Base[A]
  final class Anon[A](request : Request[A], val obj : O) extends SiteRequest.Anon[A](request) with Site[A]
  final class Auth[A](request : Request[A], identity : Account, superuser : Boolean, val obj : O) extends SiteRequest.Auth[A](request, identity, superuser) with Site[A]
}
object RequestObject {
  def getter[O](get : SiteRequest[_] => Option[O]) = new ActionRefiner[SiteRequest.Base,RequestObject[O]#Site] {
    protected def refine[A](request : SiteRequest.Base[A]) =
      get(request).fold[Either[Future[SimpleResult],RequestObject[O]#Site[A]]](
        simple(Results.NotFound))(
        o => Right(request.withObj(o)))
  }
  def permission[O <: InVolume](perm : Permission.Value = Permission.VIEW) = new ActionHandler[RequestObject[O]#Site] {
    protected def handle[A](request : RequestObject[O]#Site[A]) =
      if (request.obj.checkPermission(perm)(request)) None else simple(Results.Forbidden)
  }
  def check[O <: InVolume](get : SiteRequest[_] => Option[O], perm : Permission.Value = Permission.VIEW) =
    getter(get) ~> permission(perm)
  def check[O <: InVolume](v : models.Volume.Id, get : SiteRequest[_] => Option[O], perm : Permission.Value = Permission.VIEW) =
    getter(get(_).filter(_.volumeId == v)) ~> permission(perm)
}

object SiteAction extends ActionCreator[SiteRequest.Base] {
  private[this] def getUser(request : Request[_]) : Option[Account] =
    request.session.get("user").flatMap(Maybe.toInt _).flatMap(models.Account.get_ _)

  private[this] def getSuperuser(request : Request[_]) : Boolean =
    request.session.get("superuser").flatMap(Maybe.toLong _).exists(_ > System.currentTimeMillis)

  def invokeBlock[A](request : Request[A], block : SiteRequest.Base[A] => Future[SimpleResult]) =
    block(SiteRequest(request, getUser(request), getSuperuser(request)))

  object Auth extends ActionRefiner[SiteRequest,SiteRequest.Auth] {
    protected def refine[A](request : SiteRequest[A]) = request match {
      case request : SiteRequest.Auth[A] => Right(request)
      case _ => simple(Login.needLogin)
    }
  }

  val auth : ActionCreator[SiteRequest.Auth] = ~>(Auth)

  case class Access[R[_] <: SiteRequest[_]](access : Permission.Value) extends ActionHandler[R] {
    def handle[A](request : R[A]) =
      if (request.access >= access) None
      else simple(Results.Forbidden)
  }

  def access(access : Permission.Value) = auth ~> Access[SiteRequest.Auth](access)
}

class SiteController extends Controller {
  protected def isAjax[A](implicit request : Request[A]) =
    request.headers.get("X-Requested-With").equals(Some("XMLHttpRequest"))

  protected def isSecure : Boolean =
    current.configuration.getString("application.secret").exists(_ != "databrary").
      ensuring(s => s, "Application is insecure. You must set application.secret appropriately (see README).")

  protected def AOk[C : Writeable](c : C) = Future.successful(Ok[C](c))
  protected def ABad[C : Writeable](c : C) = Future.successful(BadRequest[C](c))
  protected def ARedirect(c : Call) = Future.successful(Redirect(c))
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
