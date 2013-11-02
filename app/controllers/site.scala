package controllers
import scala.language.higherKinds

import play.api._
import          Play.current
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
  def withDB(db : site.Site.DB) : SiteRequest[A]
  def withObj[O](obj : O) : RequestObject[O]#Site[A]
}

object SiteRequest {
  sealed abstract class Base[A](request : Request[A], val dbc : site.Site.DB) extends WrappedRequest[A](request) with SiteRequest[A]
  sealed class Anon[A](request : Request[A], db : site.Site.DB) extends Base[A](request, db) with AnonSite {
    def withDB(db : site.Site.DB) : Anon[A] =
      new Anon(request, db)
    def withObj[O](obj : O) : RequestObject[O]#Anon[A] = {
      object ro extends RequestObject[O]
      new ro.Anon(request, db, obj)
    }
  }
  sealed class Auth[A](request : Request[A], val identity : Account, val superuser : Boolean, db : site.Site.DB) extends Base[A](request, db) with AuthSite {
    def withDB(db : site.Site.DB) : Auth[A] =
      new Auth(request, identity, superuser, db)
    def withObj[O](obj : O) : RequestObject[O]#Auth[A] = {
      object ro extends RequestObject[O]
      new ro.Auth(request, identity, superuser, db, obj)
    }
  }

  def apply[A](request : Request[A], identity : Option[Account], superuser : Boolean, db : site.Site.DB) : SiteRequest.Base[A] =
    identity.fold[SiteRequest.Base[A]](new Anon[A](request, db))(a => new Auth[A](request, a, superuser && a.access(db) == Permission.ADMIN, db))
}

trait RequestObject[+O] {
  sealed trait Base[A] extends Request[A] {
    val obj : O
  }
  sealed trait Site[A] extends SiteRequest[A] with Base[A]
  final class Anon[A](request : Request[A], db : site.Site.DB, val obj : O) extends SiteRequest.Anon[A](request, db) with Site[A]
  final class Auth[A](request : Request[A], identity : Account, superuser : Boolean, db : site.Site.DB, val obj : O) extends SiteRequest.Auth[A](request, identity, superuser, db) with Site[A]
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
  private[this] def getUser(request : Request[_])(implicit db : site.Site.DB) : Option[Account] =
    request.session.get("user").flatMap(maybe.toInt _).flatMap(models.Account.get_ _)

  private[this] def getSuperuser(request : Request[_]) : Boolean =
    request.session.get("superuser").flatMap(maybe.toLong _).exists(_ > System.currentTimeMillis)

  def invokeBlock[A](request : Request[A], block : SiteRequest.Base[A] => Future[SimpleResult]) = {
    val dbc = Site.dbPool
    block(SiteRequest(request, getUser(request)(dbc), getSuperuser(request), dbc))
  }

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
  def isAjax[A](implicit request : Request[A]) =
    request.headers.get("X-Requested-With").equals(Some("XMLHttpRequest"))

  def isSecure : Boolean =
    current.configuration.getString("application.secret").exists(_ != "databrary").
      ensuring(s => s, "Application is insecure. You must set application.secret appropriately (see README).")
}

object Site extends SiteController {
  private[controllers] val dbPool = site.Site.dbPool(current)

  def start = Login.view

  def test = Action { request =>
    implicit val dbc = dbPool
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
