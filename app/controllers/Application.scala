package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import models._

abstract class SiteRequest[A](request : Request[A], val identity : Identity) 
  extends WrappedRequest[A](request)

class AnonRequest[A](request : Request[A])
  extends SiteRequest[A](request, NoAccount)

class AccountRequest[A](request : Request[A], val account : Account)
  extends SiteRequest[A](request, account)

object SiteAction {
  private[this] def getAccount(request : Request[_]) : Option[Account] =
    request.session.get("account").flatMap { i => 
      try { Some(i.toInt) }
      catch { case e:java.lang.NumberFormatException => None }
    }.flatMap(models.Account.getId _)

  def apply(anon : AnonRequest[AnyContent] => Result, acct : AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    Action { request =>
      getAccount(request).fold(anon(new AnonRequest(request)))(a => acct(new AccountRequest(request, a)))
    }

  def apply(block : SiteRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(block(_), block(_))
}

object AccountAction {
  def apply(block : AccountRequest[AnyContent] => Result) : Action[AnyContent] = 
    SiteAction(Login.needLogin(_), block(_))
}

object Application extends Controller {
  
  def start = SiteAction(request => Ok(Login.viewLogin()(request)), implicit request =>
    Ok(views.html.entity(request.account.entity)))

}
