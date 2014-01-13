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
import dbrary._
import site._
import models._

private[controllers] object LoginController extends LoginController {
  private[controllers] def needed(message : String)(implicit request : SiteRequest[_]) : SimpleResult = {
    val msg = Messages(message)
    if (request.isApi) Forbidden(msg)
    else Forbidden(LoginHtml.viewLogin(msg))
  }
}

private[controllers] sealed class LoginController extends SiteController {

  protected def json(implicit site : SiteRequest.Auth[_]) =
    site.identity.json ++
    JsonObject.flatten(
      Some('access -> site.access),
      if (site.access == Permission.ADMIN) Some('superuser -> new Timestamp(site.session.get("superuser").flatMap(Maybe.toLong _).getOrElse(0L))) else None
    )

  private[controllers] def login(a : Account)(implicit request : SiteRequest[_]) : Future[SimpleResult] = {
    Audit.actionFor(Audit.Action.open, a.id, dbrary.Inet(request.remoteAddress))
    SessionToken.create(a).map { token =>
      (if (request.isApi) Ok(json(new SiteRequest.Auth(request, token)))
      else Redirect(routes.PartyHtml.view(a.id)))
        .withSession("session" -> token.id)
    }
  }

  type LoginForm = Form[(Option[String],String,String)]
  protected val loginForm : LoginForm = Form(tuple(
    "email" -> optional(email),
    "password" -> text,
    "openid" -> text(0, 256)
  ))

  def post = SiteAction.async { implicit request =>
    val form = loginForm.bindFromRequest
    form.fold(
      form => ABadForm(views.html.party.login(_ : LoginForm), form),
      { case (email, password, openid) =>
        macros.Async.flatMap(email, Account.getEmail _).flatMap { acct =>
        def error : Future[SimpleResult] =
          macros.Async.foreach[Account, Unit](acct, a => Audit.actionFor(Audit.Action.attempt, a.id, dbrary.Inet(request.remoteAddress)).execute).flatMap { _ =>
            ABadForm(views.html.party.login(_ : LoginForm), form.copy(data = form.data.updated("password", "")).withGlobalError(Messages("login.bad")))
          }
        if (!password.isEmpty) {
          acct.filter(a => !a.password.isEmpty && BCrypt.checkpw(password, a.password)).fold(error)(login)
        } else if (!openid.isEmpty)
          OpenID.redirectURL(openid, routes.LoginHtml.openID(email.getOrElse("")).absoluteURL(), realm = Some("http://" + request.host))
            .map(Redirect(_))
            .recover { case e : OpenIDError => InternalServerError(LoginHtml.viewLogin(e.toString)) }
        else
          acct.filterNot(_ => isSecure).fold(error)(login)
        }
      }
    )
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
    (if (request.isApi) Ok("")
    else Redirect(routes.VolumeHtml.search))
      .withNewSession
  }

  def superuserOn = SiteAction.access(Permission.ADMIN) { implicit request =>
    val expires = System.currentTimeMillis + 60*60*1000
    Audit.action(Audit.Action.superuser)
    (if (request.isApi) Ok(json + ('superuser -> new Timestamp(expires)))
    else Redirect(request.headers.get(REFERER).getOrElse(routes.VolumeHtml.search.url)))
      .withSession(session + ("superuser" -> expires.toString))
  }

  def superuserOff = SiteAction.access(Permission.ADMIN) { implicit request =>
    (if (request.isApi) Ok(json - "superuser")
    else Redirect(request.headers.get(REFERER).getOrElse(routes.VolumeHtml.search.url)))
      .withSession(session - "superuser")
  }

  def get = SiteAction.access(Permission.VIEW) { implicit request =>
    Ok(json)
  }
}

object LoginHtml extends LoginController {
    def viewLogin()(implicit request: SiteRequest[_]) : templates.Html =
      views.html.party.login(loginForm)
    def viewLogin(err : String)(implicit request: SiteRequest[_]) : templates.Html =
      views.html.party.login(loginForm.withGlobalError(err))

    def view = SiteAction { implicit request =>
      request.user.fold(Ok(viewLogin()))(u => Redirect(u.party.pageURL))
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
}

object LoginApi extends LoginController {
}
