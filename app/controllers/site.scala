package controllers

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

class UserRequest[A](request : Request[A], account : Account, db : util.Site.DB) extends SiteRequest[A](request, account, db) with AuthSite {
  override val identity = account
}

object SiteRequest {
  def apply[A](request : Request[A], identity : Option[Account])(implicit db : util.Site.DB) : SiteRequest[A] =
    identity.fold(new AnonRequest[A](request, db) : SiteRequest[A])(new UserRequest[A](request, _, db))
}

object SiteAction extends ActionHandler[SiteRequest] {
  private[this] def getUser(request : Request[_])(implicit db : util.Site.DB) : Option[Account] =
    request.session.get("user").flatMap { i =>
      try { Some(models.Account.asId(i.toInt)) }
      catch { case e:java.lang.NumberFormatException => None }
    }.flatMap(models.Account.get_ _)

  def invokeBlock[A](request : Request[A], block : SiteRequest[A] => Future[SimpleResult]) =
    db.DB.withTransaction { implicit db =>
      block(SiteRequest(request, getUser(request)))
    }

  object user extends SimpleRefiner[UserRequest] {
    protected def refineSimple[B](request : SiteRequest[B]) = request match {
      case request : UserRequest[B] => Right(request)
      case _ => Left(Login.needLogin)
    }
  }

  def access(access : Permission.Value) : ActionHandler[UserRequest] = new user.SimpleHandler {
    protected def handleSimple[B](request : UserRequest[B]) =
      if (request.access >= access) None
      else Some(Results.Forbidden)
  }

    /*
  object user extends ActionBuilder[UserRequest] {
    def invokeBlock[A](request : Request[A], block : UserRequest[A] => Future[SimpleResult]) =
      SiteAction.invokeBlock(request, {
        case request : UserRequest[A] => block(request)
        case _ => Future.successful(Login.needLogin)
      } : SiteRequest[A] => Future[SimpleResult])
  }

  def access(access : Permission.Value) = new ActionBuilder[UserRequest] {
    def invokeBlock[A](request : Request[A], block : UserRequest[A] => Future[SimpleResult]) =
      user.invokeBlock(request, { request : UserRequest[A] =>
        if (request.access >= access) block(request)
        else Future.successful(Results.Forbidden)
      })
  }
  */
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
