package controllers
import scala.language.higherKinds

import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import scala.concurrent.Future
import dbrary._
import util._
import models._

abstract class SiteRequest[A](request : Request[A], val identity : Party, val db : util.Site.DB) extends WrappedRequest[A](request) with Site {
  def clientIP = Inet(request.remoteAddress)
}

class AnonRequest[A](request : Request[A], db : util.Site.DB) extends SiteRequest[A](request, models.Party.Nobody, db) {
  override def user = None
}

class UserRequest[A](request : Request[A], override val identity : Account, db : util.Site.DB) extends SiteRequest[A](request, identity, db) with AuthSite {
}

object SiteRequest {
  def apply[A](request : Request[A], identity : Option[Account])(implicit db : util.Site.DB) : SiteRequest[A] =
    identity.fold(new AnonRequest[A](request, db) : SiteRequest[A])(new UserRequest[A](request, _, db))
}

class RequestObject[O] {
  case class T[A](request : SiteRequest[A], obj : O) extends SiteRequest[A](request, request.identity, request.db)
}
object RequestObject {
  def apply[O,A](request : SiteRequest[A], obj : O) : RequestObject[O]#T[A] = {
    object RO extends RequestObject[O]
    new RO.T[A](request, obj)
  }
  def getter[O](get : SiteRequest[_] => Option[O]) = new ActionRefiner[SiteRequest,RequestObject[O]#T] {
    protected def refine[A](request : SiteRequest[A]) =
      get(request) match {
        case None => simple(Results.NotFound)
        case Some(o) => Right(RequestObject[O,A](request, o))
      }
  }
  def permission[O <: InVolume](perm : Permission.Value = Permission.VIEW) = new ActionHandler[RequestObject[O]#T] {
    protected def handle[A](request : RequestObject[O]#T[A]) =
      if (request.obj.permission < perm) simple(Results.Forbidden)
      else None
  }
  def check[O <: InVolume](get : SiteRequest[_] => Option[O], perm : Permission.Value = Permission.VIEW) =
    getter(get) ~> permission(perm)
  def check[O <: InVolume](v : models.Volume.Id, get : SiteRequest[_] => Option[O], perm : Permission.Value = Permission.VIEW) =
    getter(get(_).filter(_.volumeId == v)) ~> permission(perm)
}

object SiteAction extends ActionCreator[SiteRequest] {
  private[this] def getUser(request : Request[_])(implicit db : util.Site.DB) : Option[Account] =
    request.session.get("user").flatMap { i =>
      try { Some(models.Account.asId(i.toInt)) }
      catch { case e:java.lang.NumberFormatException => None }
    }.flatMap(models.Account.get_ _)

  def invokeBlock[A](request : Request[A], block : SiteRequest[A] => Future[SimpleResult]) =
    db.DB.withTransaction { implicit db =>
      block(SiteRequest(request, getUser(request)))
    }

  object User extends ActionRefiner[SiteRequest,UserRequest] {
    protected def refine[A](request : SiteRequest[A]) = request match {
      case request : UserRequest[A] => Right(request)
      case _ => simple(Login.needLogin)
    }
  }

  val user : ActionCreator[UserRequest] = ~>(User)

  case class Access[R[_] <: SiteRequest[_]](access : Permission.Value) extends ActionHandler[R] {
    def handle[A](request : R[A]) =
      if (request.access >= access) None
      else simple(Results.Forbidden)
  }

  def access(access : Permission.Value) = user ~> Access[UserRequest](access)
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
