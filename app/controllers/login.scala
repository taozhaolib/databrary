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
import util._
import models._

object Login extends Controller {

  private[this] val loginForm = Form(tuple(
    "username" -> text(0, 32),
    "openid" -> text(0, 256)
  ))

  def viewLogin(err : Option[String] = None) : templates.Html = 
    views.html.login(err.fold(loginForm)(loginForm.withGlobalError(_)))
  def viewLogin(err : String) : templates.Html = 
    viewLogin(Some(err))
  def needLogin =
    Forbidden(viewLogin(Some(Messages("login.noCookie"))))

  def view = Action { request => 
    Ok(viewLogin())
  }

  private[this] def login(a : Account)(implicit request : Request[_], db : util.Site.DB) = {
    implicit val arequest = new UserRequest(request, a, db)
    Audit.action(AuditAction.login)
    Redirect(routes.Party.view(a.id)).withSession("user" -> a.id.unId.toString)
  }
  
  def post = Action { implicit request =>
    val form = loginForm.bindFromRequest
    form.fold(
      form => BadRequest(views.html.login(form)),
      { case (username, openid) => 
        if (openid.isEmpty)
          if (!username.isEmpty && !Site.isSecure)
            DB.withTransaction { implicit db =>
              Account.getUsername(username).fold(
                BadRequest(views.html.login(form.withError("username", "error.invalid"))) : Result
              )(login)
            }
          else
            BadRequest(views.html.login(form.withError("openid", "error.required")))
        else AsyncResult {
          OpenID.redirectURL(openid, routes.Login.openID(username).absoluteURL(), realm = Some("http://" + request.host)).extend1 {
            case Redeemed(url) => Redirect(url)
            case Thrown(t) => InternalServerError(viewLogin(t.toString))
          }
        }
      }
    )
  }

  def openID(username : String) = Action { implicit request =>
    AsyncResult(OpenID.verifiedId.extend1(
      { 
        case Redeemed(info) => DB.withTransaction { implicit db =>
          Account.getOpenid(info.id, maybe(username)).fold(
            BadRequest(viewLogin(Messages("login.openID.notFound", info.id))) : Result
          )(login)
        }
        case Thrown(t) => InternalServerError(viewLogin(t.toString))
      }
    ))
  }

  def logout = SiteAction { implicit request =>
    if (request.isInstanceOf[UserRequest[_]])
      Audit.action(AuditAction.logout)
    Ok(viewLogin(Messages("login.logout"))).withNewSession
  }
}
