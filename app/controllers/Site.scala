package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import          db.slick
import             slick.DB
import             slick.Config.driver.simple.{Session=>DBConnection,_}
import util._
import models._

abstract class SiteRequest[A](request : Request[A], val identity : Identity, val db : DBConnection)
  extends WrappedRequest[A](request) with Site {
  def clientIP = Inet(request.remoteAddress)
}

class AnonRequest[A](request : Request[A], db : DBConnection)
  extends SiteRequest[A](request, Identity.Nobody, db)

class UserRequest[A](request : Request[A], val user : User, db : DBConnection)
  extends SiteRequest[A](request, user, db)

object SiteAction {
  private[this] def getUser(request : Request[_])(implicit db : DBConnection) : Option[User] =
    request.session.get("user").flatMap { i => 
      try { Some(i.toInt) }
      catch { case e:java.lang.NumberFormatException => None }
    }.flatMap(User.get _)

  def apply(anon : AnonRequest[AnyContent] => Result, user : UserRequest[AnyContent] => Result) : Action[AnyContent] =
    Action { request => DB.withTransaction { implicit db =>
      getUser(request).fold(anon(new AnonRequest(request, db)))(u => user(new UserRequest(request, u, db)))
    } }

  def apply(block : SiteRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(block(_), block(_))
}

object UserAction {
  def apply(block : UserRequest[AnyContent] => Result) : Action[AnyContent] = 
    SiteAction(_ => Login.needLogin, block(_))
}

class SiteController extends Controller {
  implicit def db(implicit request : SiteRequest[_]) = request.db
}

object Site extends SiteController {
  
  def start = SiteAction(request => Ok(Login.viewLogin()), implicit request =>
    Ok(views.html.entity(request.identity)))

  def test = Action { request => DB.withSession { implicit db =>
    Ok("Ok")
  } }

}
