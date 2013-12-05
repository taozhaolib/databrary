package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          libs.openid._
import          libs.concurrent._
import                          Execution.Implicits.defaultContext
import          i18n.Messages
import scala.concurrent.Future
import org.mindrot.jbcrypt.BCrypt
import macros._
import site._
import models._

object Login extends SiteController {

  type LoginForm = Form[(Option[String],String,String)]
  private[this] val loginForm : LoginForm = Form(tuple(
    "email" -> optional(email),
    "password" -> text,
    "openid" -> text(0, 256)
  ))

  def viewLogin()(implicit request: SiteRequest[_]) : templates.Html =
    views.html.party.login(loginForm)
  def viewLogin(err : String)(implicit request: SiteRequest[_]) : templates.Html =
    views.html.party.login(loginForm.withGlobalError(err))
  def needLogin(implicit request: SiteRequest[_]) =
    Forbidden(viewLogin(Messages("login.noCookie")))

  def view = SiteAction { implicit request =>
    request.user.fold(Ok(viewLogin()))(u => Redirect(u.party.pageURL))
  }

  private[controllers] def login(a : Account)(implicit request : Request[_]) : Future[SimpleResult] = {
    Audit.actionFor(Audit.Action.open, a.id, dbrary.Inet(request.remoteAddress))
    SessionToken.create(a).map { token =>
      Redirect(routes.Party.view(a.id)).withSession("session" -> token)
    }
  }

  def post = SiteAction.async { implicit request =>
    val form = loginForm.bindFromRequest
    form.fold(
      form => ABadRequest(views.html.party.login(form)),
      { case (email, password, openid) =>
        macros.Async.flatMap(email, Account.getEmail _).flatMap { acct =>
        def error : Future[SimpleResult] = macros.Async {
          acct.foreach(a => Audit.actionFor(Audit.Action.attempt, a.id, dbrary.Inet(request.remoteAddress)))
          BadRequest(views.html.party.login(form.copy(data = form.data.updated("password", "")).withGlobalError(Messages("login.bad"))))
        }
        if (!password.isEmpty) {
          acct.filter(a => !a.password.isEmpty && BCrypt.checkpw(password, a.password)).fold(error)(login)
        } else if (!openid.isEmpty)
          OpenID.redirectURL(openid, routes.Login.openID(email.getOrElse("")).absoluteURL(), realm = Some("http://" + request.host))
            .map(Redirect(_))
            .recover { case e : OpenIDError => InternalServerError(viewLogin(e.toString)) }
        else
          acct.filterNot(_ => isSecure).fold(error)(login)
        }
      }
    )
  }

  def openID(email : String) = SiteAction.async { implicit request =>
    val em = Maybe(email).opt
    OpenID.verifiedId
      .flatMap { info =>
        Account.getOpenid(info.id, em).flatMap(_.fold(
          ABadRequest(views.html.party.login(loginForm.fill((em, "", info.id)).withError("openid", "login.openID.notFound")))
        )(login))
      }.recover { case e : OpenIDError => InternalServerError(viewLogin(e.toString)) }
  }

  def logout = SiteAction { implicit request =>
    request match {
      case auth : SiteRequest.Auth[_] =>
        for {
          _ <- auth.token.remove
          _ <- Audit.action(Audit.Action.close)
        } yield {}
      case _ =>
    }
    Redirect(routes.Static.index).withNewSession
  }

  def superuserOn = SiteAction.access(Permission.ADMIN) { implicit request =>
    Audit.action(Audit.Action.superuser)
    Redirect(request.headers.get(REFERER).getOrElse(routes.Static.index.url)).withSession(session + ("superuser" -> (System.currentTimeMillis + 60*60*1000).toString))
  }

  def superuserOff = SiteAction.access(Permission.ADMIN) { implicit request =>
    Redirect(request.headers.get(REFERER).getOrElse(routes.Static.index.url)).withSession(session - "superuser")
  }
}
