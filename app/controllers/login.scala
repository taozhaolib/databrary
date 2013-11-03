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

  def viewLogin(err : Option[String] = None) : templates.Html =
    views.html.account.login(err.fold(loginForm)(loginForm.withGlobalError(_)))
  def viewLogin(err : String) : templates.Html =
    viewLogin(Some(err))
  def needLogin =
    Forbidden(viewLogin(Some(Messages("login.noCookie"))))

  def view = SiteAction { implicit request =>
    Ok(request.user.fold(viewLogin())(p => views.html.party.view(p)(request.withObj(p))))
  }

  def ajaxView = SiteAction { implicit request =>
    Ok(request.user.fold(views.html.modal.login(loginForm))(p => views.html.modal.profile(p)(request.withObj(p))))
  }

  private[controllers] def login(a : Account)(implicit request : Request[_], db : site.Site.DB) = {
    Audit.actionFor(Audit.Action.open, a.id, dbrary.Inet(request.remoteAddress))
    Redirect(routes.Party.view(a.id)).withSession("user" -> a.id.toString)
  }

  def post = Action.async { implicit request =>
    val form = loginForm.bindFromRequest
    form.fold(
      form => Future.successful(BadRequest(views.html.account.login(form))),
      { case (email, password, openid) =>
        implicit val dbc = Site.dbPool
        val acct = email.flatMap(Account.getEmail _)
        def error() : SimpleResult = {
          acct.foreach(a => Audit.actionFor(Audit.Action.attempt, a.id, dbrary.Inet(request.remoteAddress)))
          BadRequest(views.html.account.login(form.copy(data = form.data.updated("password", "")).withGlobalError(Messages("login.bad"))))
        }
        if (!password.isEmpty) {
          Future.successful(acct.filter(a => !a.password.isEmpty && BCrypt.checkpw(password, a.password)).fold(error)(login))
        } else if (!openid.isEmpty)
          OpenID.redirectURL(openid, routes.Login.openID(email.getOrElse("")).absoluteURL(), realm = Some("http://" + request.host))
            .map(Redirect(_))
            .recover { case e : OpenIDError => InternalServerError(viewLogin(e.toString)) }
        else
          Future.successful(acct.filterNot(_ => isSecure).fold(error)(login))
      }
    )
  }

  def openID(email : String) = Action.async { implicit request =>
    OpenID.verifiedId
      .map { info =>
        implicit val dbc = Site.dbPool
        Account.getOpenid(info.id, Maybe.opt(email)).fold[SimpleResult](
          BadRequest(views.html.account.login(loginForm.fill((Maybe.opt(email), "", info.id)).withError("openid", "login.openID.notFound")))
        )(login)
      }.recover { case e : OpenIDError => InternalServerError(viewLogin(e.toString)) }
  }

  def logout = SiteAction { implicit request =>
    if (request.user.isDefined)
      Audit.action(Audit.Action.close)
    Redirect(routes.Static.index).withNewSession
  }

  def superuserOn = SiteAction.access(Permission.ADMIN) { implicit request =>
    Audit.action(Audit.Action.superuser)
    Redirect(request.headers.get(REFERER).getOrElse(routes.Static.index.url)).withSession("user" -> request.identity.id.toString, "superuser" -> (System.currentTimeMillis + 60*60*1000).toString)
  }

  def superuserOff = SiteAction.access(Permission.ADMIN) { implicit request =>
    Redirect(request.headers.get(REFERER).getOrElse(routes.Static.index.url)).withSession("user" -> request.identity.id.toString)
  }
}
