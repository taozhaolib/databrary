package controllers

import util._
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
import models._

object Login extends Controller {

  private[this] val loginForm = Form(tuple(
    "username" -> text,
    "openid" -> nonEmptyText
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
  
  def post = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      form => BadRequest(views.html.login(form)),
      { case (username, openid) => AsyncResult(OpenID.redirectURL(openid, routes.Login.openID(username).absoluteURL(), realm = Some("http://" + request.host)).extend1(
        {
          case Redeemed(url) => Redirect(url)
          case Thrown(t) => InternalServerError(viewLogin(t.toString))
        }
      ))}
    )
  }

  def openID(username : String) = Action { implicit request =>
    AsyncResult(OpenID.verifiedId.extend1(
      { 
        case Redeemed(info) => DB.withConnection { implicit db =>
          User.getOpenid(info.id, maybe(username)).map { a =>
            implicit val arequest = new UserRequest(request, a, db)
            Audit.add(AuditAction.login)
            Redirect(routes.Entity.view(a.id)).withSession("user" -> a.id.unId.toString)
          }.getOrElse(
            BadRequest(viewLogin(Messages("login.openID.notFound", info.id)))
          )
        }
        case Thrown(t) => InternalServerError(viewLogin(t.toString))
      }
    ))
  }

  def logout = SiteAction { implicit request =>
    if (request.isInstanceOf[UserRequest[_]])
      Audit.add(AuditAction.logout)
    Ok(viewLogin(Messages("login.logout"))).withNewSession
  }
}
