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

  def apply(noaccount : Request[AnyContent] => Result, block : AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    Action { request =>
      getAccount(request).fold(noaccount(request)) {
        account => block(new AccountRequest[AnyContent](request, account))
      }
    }

  def apply(noaccount : => Result, block : AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(_ => noaccount, block)

  def apply(block : AccountRequest[AnyContent] => Result) : Action[AnyContent] =
    apply(Application.viewLogin(Messages("login.noCookie")), block)
}

object Application extends Controller {
  
  def ddl = Action {
    Ok(views.html.ddl(Entity.ddl ++ Trust.ddl ++ Account.ddl))
  }

  def start = AccountAction(viewLogin,
    { request : AccountRequest[AnyContent] => viewHome(request) })

  val loginForm = Form(tuple(
    "username" -> text,
    "openid" -> nonEmptyText
  ))

  def viewLogin = Ok(views.html.login(loginForm))
  def viewLogin(err : String) = Ok(views.html.login(loginForm.withGlobalError(err)))

  def login = Action { request => viewLogin }
  
  def postLogin = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      form => BadRequest(views.html.login(form)),
      { case (username, openid) => AsyncResult(OpenID.redirectURL(openid, routes.Application.openID(username).absoluteURL(), realm = Some("http://" + request.host)).extend1(
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
            val qao = Query(Account).filter(_.openid === info.id)
            (if (username.isEmpty) qao else qao.filter(_.username === username)).firstOption
          }.map { a =>
            Redirect(routes.Application.home).withSession("account" -> a.id.toString)
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

  def home = AccountAction { viewHome(_) }

  val accountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))

  def accountFormFill(a : Account) = accountForm.fill((a.email, a.openid.getOrElse("")))

  def authorizeForm(parent : Entity) = Form(mapping(
      "name" -> optional(text),
      "child" -> number,
      "parent" -> ignored(parent.id),
      "access" -> number(min=0, max=SitePermission.maxId-1),
      "delegate" -> number(min=0, max=UserPermission.maxId-1),
      "expires" -> sqlDate
    )((name, child, parent, access, delegate, expires) => Trust(child, parent, SitePermission(access), UserPermission(delegate), Some(new java.sql.Timestamp(expires.getTime)))
    )(t => t.expires.map(e => (Some(t.childEntity.name), t.child, t.parent, t.access.id, t.delegate.id, new java.sql.Date(e.getTime))))
  )

  def viewHome(implicit request : AccountRequest[_]) = {
    val acct = request.account
    val form = authorizeForm(acct.entity)
    Ok(views.html.home(acct, accountFormFill(acct), Trust.getChildren(acct.id).map(form.fill(_))))
  }
  
  def postAccount = AccountAction { implicit request =>
    accountForm.bindFromRequest.fold(
      form => BadRequest(views.html.home(request.account, form, Seq())),
      { case (email, openid) =>
        val a = request.account
        a.email = email
        a.openid = if (openid.isEmpty) None else Some(openid)
        a.commit
        Redirect(routes.Application.home)
      }
    )
  }

  def postAuthorize = AccountAction { implicit request =>
    authorizeForm(request.account.entity).bindFromRequest.fold(
      form => BadRequest(views.html.home(request.account, accountFormFill(request.account), Seq(form))),
      trust => {
        trust.commit
        Redirect(routes.Application.home)
      }
    )
  }
}
