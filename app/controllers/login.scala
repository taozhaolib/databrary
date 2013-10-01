package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          libs.openid._
import          libs.concurrent._
import                          Execution.Implicits.defaultContext
import          db.DB
import          i18n.Messages
import org.mindrot.jbcrypt.BCrypt
import util._
import models._

object Login extends SiteController {

  type LoginForm = Form[(Option[String],String,String)]
  private[this] val loginForm : LoginForm = Form(tuple(
    "email" -> optional(email),
    "password" -> text,
    "openid" -> text(0, 256)
  ))

  def viewLogin(err : Option[String] = None) : templates.Html =
    views.html.account.login(err.fold(loginForm)(loginForm.withGlobalError(_)))
  def viewLogin(err : String) : templates.Html =
    viewLogin(Some(err))
  def needLogin =
    Forbidden(viewLogin(Some(Messages("login.noCookie"))))

  def view = SiteAction(request => Ok(viewLogin()), implicit request =>
    Ok(views.html.party.view(request.identity)))

  def ajaxView = SiteAction(request => Ok(views.html.modal.login(loginForm)), implicit request =>
    Ok(views.html.modal.profile(request.identity)))

  private[this] def login(a : Account)(implicit request : Request[_], db : util.Site.DB) = {
    implicit val arequest = new UserRequest(request, a, db)
    Audit.action(Audit.Action.open)
    Redirect(routes.Party.view(a.id)).withSession("user" -> a.id.unId.toString)
  }

  def post = Action { implicit request =>
    val form = loginForm.bindFromRequest
    form.fold(
      form => BadRequest(views.html.account.login(form)),
      { case (email, password, openid) => DB.withConnection { implicit db =>
        val acct = email.flatMap(Account.getEmail _)
        def error() : Result = {
          acct.foreach(a => Audit.actionFor(Audit.Action.attempt, a.id, dbrary.Inet(request.remoteAddress)))
          BadRequest(views.html.account.login(form.copy(data = form.data.updated("password", "")).withGlobalError(Messages("login.bad"))))
        }
        if (!password.isEmpty) {
          acct.filter(a => !a.password.isEmpty && BCrypt.checkpw(password, a.password)).fold(error)(login)
        } else if (!openid.isEmpty)
          AsyncResult {
            OpenID.redirectURL(openid, routes.Login.openID(email.getOrElse("")).absoluteURL(), realm = Some("http://" + request.host)).extend1 {
              case Redeemed(url) => Redirect(url)
              case Thrown(t) => InternalServerError(viewLogin(t.toString))
            }
          }
        else
          acct.filterNot(_ => Site.isSecure).fold(error)(login)
      } }
    )
  }

  def openID(email : String) = Action { implicit request =>
    AsyncResult(OpenID.verifiedId.extend1(
      { case Redeemed(info) => DB.withConnection { implicit db =>
          Account.getOpenid(info.id, maybe(email)).fold(
            BadRequest(views.html.account.login(loginForm.fill((maybe(email), "", info.id)).withError("openid", "login.openID.notFound"))) : Result
          )(login)
      } case Thrown(t) => InternalServerError(viewLogin(t.toString))
      }
    ))
  }

  def logout = SiteAction { implicit request =>
    if (request.isInstanceOf[UserRequest[_]])
      Audit.action(Audit.Action.close)
    Redirect(routes.Static.index).withNewSession
  }
}
