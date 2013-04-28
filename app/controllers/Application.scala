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
    }.flatMap(Account.getId _)

  def apply(noaccount : Request[AnyContent] => Result, block: AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    Action { request =>
      getAccount(request).fold(noaccount(request)) {
        account => block(new AccountRequest[AnyContent](request, account))
      }
    }

  def apply(noaccount : => Result, block: AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(_ => noaccount, block)

  def apply(block: AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(Results.Ok(views.html.login(Messages("login.noCookie"))), block)
}

object Application extends Controller {
  
  def ddl = Action {
    Ok(views.html.ddl(Entity.ddl ++ Trust.ddl ++ Account.ddl))
  }

  def test = Action {
    Ok(views.html.test(Account.getUsername("dylan").orNull.access.toString))
  }

  val loginForm = Form(tuple(
    "username" -> text,
    "openid" -> text
  ))
  
  def login = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      blank => Ok(views.html.login()),
      { case (username, openid) => AsyncResult(OpenID.redirectURL(openid, routes.Application.openID(username).absoluteURL(), realm = Some("http://" + request.host)).extend1(
        {
          case Redeemed(url) => Redirect(url)
          case Thrown(t) => Ok(views.html.login(t.toString))
        }
      ))}
    )
  }

  def logout = Action { implicit request =>
    Ok(views.html.login(Messages("login.logout"))).withNewSession
  }

  def openID(username : String) = Action { implicit request =>
    AsyncResult(OpenID.verifiedId.extend1(
      { 
        case Redeemed(info) =>
          DB.withSession { implicit session =>
            val qao = Query(Account).filter(_.openid === info.id)
            (if (username.isEmpty) qao else qao.filter(_.username === username)).firstOption
          }.map { a =>
            Redirect(routes.Application.home).withSession("account" -> a.id.toString)
          }.getOrElse(
            Ok(views.html.login(Messages("login.openID.notFound", info.id)))
          ) 
        case Thrown(t) => Ok(views.html.login(t.toString))
      }
    ))
  }

  def home = AccountAction { implicit request =>
    Ok(views.html.home(request.account))
  }

  val accountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))

  def accountFormFill(a : Account) = accountForm.fill((a.email, a.openid.getOrElse("")))
  
  def account = AccountAction { implicit request =>
    accountForm.bindFromRequest.fold(
      blank => Ok(views.html.home(request.account)),
      { case (email, openid) => 
        val a = request.account
        a.email = email
        a.openid = if (openid.isEmpty) None else Some(openid)
        a.commit
        Redirect(routes.Application.home)
      }
    )
  }
}
