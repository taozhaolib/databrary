package controllers
import scala.language.higherKinds

import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import scala.concurrent.Future
import dbrary._
import site._
import models._

sealed trait SiteRequest[A] extends Request[A] with Site {
  def clientIP = Inet(remoteAddress)
}

object SiteRequest {
  sealed abstract class Base[A](request : Request[A], db : site.Site.DB) extends WrappedRequest[A](request) with SiteRequest[A]
  final case class Anon[A](request : Request[A], db : site.Site.DB) extends Base[A](request, db) with AnonSite
  final case class Auth[A](request : Request[A], identity : Account, db : site.Site.DB) extends Base[A](request, db) with AuthSite

  def apply[A](request : Request[A], identity : Option[Account])(implicit db : site.Site.DB) : SiteRequest.Base[A] =
    identity.fold[SiteRequest.Base[A]](Anon[A](request, db))(Auth[A](request, _, db))
}

trait RequestObject[+O] {
  sealed trait Base[A] extends Request[A] {
    val obj : O
  }
  sealed trait Site[A] extends SiteRequest[A] with Base[A]
  final case class Anon[A](request : Request[A], db : site.Site.DB, obj : O) extends WrappedRequest[A](request) with Site[A] with AnonSite
  final case class Auth[A](request : Request[A], identity : Account, db : site.Site.DB, obj : O) extends WrappedRequest[A](request) with Site[A] with AuthSite
}
object RequestObject {
  def apply[O,A](request : SiteRequest.Base[A], obj : O) : RequestObject[O]#Site[A] = {
    object RO extends RequestObject[O]
    request match {
      case SiteRequest.Anon(request, db) => new RO.Anon[A](request, db, obj)
      case SiteRequest.Auth(request, identity, db) => new RO.Auth[A](request, identity, db, obj)
    }
  }
  def getter[O](get : SiteRequest[_] => Option[O]) = new ActionRefiner[SiteRequest.Base,RequestObject[O]#Site] {
    protected def refine[A](request : SiteRequest.Base[A]) =
      get(request) match {
        case None => simple(Results.NotFound)
        case Some(o) => Right(RequestObject[O,A](request, o))
      }
  }
  def permission[O <: InVolume](perm : Permission.Value = Permission.VIEW) = new ActionHandler[RequestObject[O]#Site] {
    protected def handle[A](request : RequestObject[O]#Site[A]) =
      if (request.obj.permission < perm) simple(Results.Forbidden)
      else None
  }
  def check[O <: InVolume](get : SiteRequest[_] => Option[O], perm : Permission.Value = Permission.VIEW) =
    getter(get) ~> permission(perm)
  def check[O <: InVolume](v : models.Volume.Id, get : SiteRequest[_] => Option[O], perm : Permission.Value = Permission.VIEW) =
    getter(get(_).filter(_.volumeId == v)) ~> permission(perm)
}

object SiteAction extends ActionCreator[SiteRequest.Base] {
  private[this] def getUser(request : Request[_])(implicit db : site.Site.DB) : Option[Account] =
    request.session.get("user").flatMap { i =>
      try { Some(models.Account.asId(i.toInt)) }
      catch { case e:java.lang.NumberFormatException => None }
    }.flatMap(models.Account.get_ _)

  def invokeBlock[A](request : Request[A], block : SiteRequest.Base[A] => Future[SimpleResult]) =
    db.DB.withTransaction { implicit db =>
      block(SiteRequest(request, getUser(request)))
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
      ensuring(_ || !Play.isProd, "Running insecure in production")
}

object Site extends SiteController {
  def start = Login.view

  def test = Action { request => db.DB.withConnection { implicit db =>
    Ok("Ok")
  } }

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
