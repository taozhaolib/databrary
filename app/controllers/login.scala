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

  protected def json(implicit site : SiteRequest[_]) =
    site.identity.json ++
    JsonObject.flatten(
      Some('access -> site.access.group),
      if (site.access.isAdmin) Some('superuser -> site.session.get("superuser").flatMap(Maybe.toLong _).map(_ - System.currentTimeMillis).filter(_ > 0).getOrElse(0L)) else None
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
          OpenID.redirectURL(openid, routes.LoginHtml.openID(email.getOrElse("")).absoluteURL(true), realm = Some("http://" + request.host))
            .map(Redirect(_))
            .recover { case e : OpenIDError => InternalServerError(LoginHtml.viewLogin(e.toString)) }
        else
	  error
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
    else Redirect(routes.Site.start))
      .withNewSession
  }

  private final val superuserTime : Long = 60*60*1000
  def superuserOn = SiteAction.rootAccess() { implicit request =>
    val expires = System.currentTimeMillis + superuserTime
    Audit.action(Audit.Action.superuser)
    (if (request.isApi) Ok(json + ('superuser -> superuserTime))
    else Redirect(request.headers.get(REFERER).getOrElse(routes.Site.start.url)))
      .withSession(session + ("superuser" -> expires.toString))
  }

  def superuserOff = SiteAction { implicit request =>
    (if (request.isApi) Ok(json - "superuser")
    else Redirect(request.headers.get(REFERER).getOrElse(routes.Site.start.url)))
      .withSession(session - "superuser")
  }

  type RegistrationMapping = (String, String, String, Boolean)
  type RegistrationForm = Form[RegistrationMapping]
  protected val registrationForm : RegistrationForm = Form(tuple(
    "name" -> nonEmptyText,
    "email" -> email,
    "affiliation" -> text,
    "agreement" -> checked(Messages("agreement.required"))
  ))

  def register = SiteAction.async { implicit request =>
    def Error(form : RegistrationForm) =
      throw new BadFormException[RegistrationMapping](views.html.party.register(_))(form)
    val form = registrationForm.bindFromRequest
    form.fold(Error _, {
      case (name, email, affiliation, _) =>
	for {
	  p <- Party.create(
	    name = name,
	    affiliation = Maybe(affiliation).opt)
          a <- Account.create(p, email = email)
	  _ <- controllers.Token.newPassword(Right(a), "register")
	} yield (Ok("sent"))
    })
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

  def registration = SiteAction { implicit request =>
    Ok(views.html.party.register(registrationForm))
  }
}

object LoginApi extends LoginController {
  def get = SiteAction { implicit request =>
    Ok(json)
  }
}
