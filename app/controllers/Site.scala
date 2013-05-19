package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import          db.slick
import             slick.DB
import             slick.Config.driver.simple.{Session=>DBConnection,_}
import models._

abstract class SiteRequest[A](request : Request[A], val identity : Identity, val db : DBConnection)
  extends WrappedRequest[A](request)

class AnonRequest[A](request : Request[A], db : DBConnection)
  extends SiteRequest[A](request, NoAccount, db)

class AccountRequest[A](request : Request[A], val account : Account, db : DBConnection)
  extends SiteRequest[A](request, account, db)

object SiteAction {
  private[this] def getAccount(request : Request[_])(implicit db : DBConnection) : Option[Account] =
    request.session.get("account").flatMap { i => 
      try { Some(i.toInt) }
      catch { case e:java.lang.NumberFormatException => None }
    }.flatMap(models.Account.getId _)

  def apply(anon : AnonRequest[AnyContent] => Result, acct : AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    Action { request => DB.withSession { implicit db =>
      getAccount(request).fold(anon(new AnonRequest(request, db)))(a => acct(new AccountRequest(request, a, db)))
    } }

  def apply(block : SiteRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(block(_), block(_))
}

object AccountAction {
  def apply(block : AccountRequest[AnyContent] => Result) : Action[AnyContent] = 
    SiteAction(_ => Login.needLogin, block(_))
}

class SiteController extends Controller {
  implicit def identity(implicit request : SiteRequest[_]) = request.identity
  implicit def account(implicit request : AccountRequest[_]) = request.account
  implicit def db(implicit request : SiteRequest[_]) = request.db
}

object Site extends SiteController {
  
  def start = SiteAction(request => Ok(Login.viewLogin()), implicit request =>
    Ok(views.html.entity(request.account.entity)))

}
