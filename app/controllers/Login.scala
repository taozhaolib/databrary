package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          libs.openid._
import          libs.concurrent._
import                          Execution.Implicits.defaultContext
import          db.slick.DB
import          db.slick.Config.driver.simple._
import          i18n.Messages
import models._

class AccountRequest[A](request : Request[A], val account : Account) 
  extends WrappedRequest[A](request)

object AccountAction {
  def getAccount(request : Request[_]) : Option[Account] =
    request.session.get("account").flatMap { i => 
      try { Some(i.toInt) }
      catch { case e:java.lang.NumberFormatException => None }
    }.flatMap(models.Account.getId _)

  def apply(noaccount : Request[AnyContent] => Result, block : AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    Action { request =>
      getAccount(request).fold(noaccount(request)) {
        account => block(new AccountRequest[AnyContent](request, account))
      }
    }

  def apply(noaccount : => Result, block : AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(_ => noaccount, block)

  def apply(block : AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(Login.viewLogin(Messages("login.noCookie")), block)
}

object Login extends Controller {

  private val loginForm = Form(tuple(
    "username" -> text,
    "openid" -> nonEmptyText
  ))

  def viewLogin = Ok(views.html.login(loginForm))
  def viewLogin(err : String) = Ok(views.html.login(loginForm.withGlobalError(err)))

  def login = Action { request => viewLogin }
  
  def postLogin = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      form => BadRequest(views.html.login(form)),
      { case (username, openid) => AsyncResult(OpenID.redirectURL(openid, routes.Login.openID(username).absoluteURL(), realm = Some("http://" + request.host)).extend1(
        {
          case Redeemed(url) => Redirect(url)
          case Thrown(t) => viewLogin(t.toString)
        }
      ))}
    )
  }

  def openID(username : String) = Action { implicit request =>
    AsyncResult(OpenID.verifiedId.extend1(
      { 
        case Redeemed(info) =>
          DB.withSession { implicit session =>
            val qao = Query(models.Account).filter(_.openid === info.id)
            (if (username.isEmpty) qao else qao.filter(_.username === username)).firstOption
          }.map { a =>
            Redirect(routes.Account.home).withSession("account" -> a.id.toString)
          }.getOrElse(
            viewLogin(Messages("login.openID.notFound", info.id))
          ) 
        case Thrown(t) => viewLogin(t.toString)
      }
    ))
  }

  def logout = Action { request =>
    viewLogin(Messages("login.logout")).withNewSession
  }
}
