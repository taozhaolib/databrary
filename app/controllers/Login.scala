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

object Login extends Controller {

  private[this] val loginForm = Form(tuple(
    "username" -> text,
    "openid" -> nonEmptyText
  ))

  def viewLogin(err : Option[String] = None)(implicit request : Request[_]) : templates.Html = 
    views.html.login(err.fold(loginForm)(loginForm.withGlobalError(_)))(new AnonRequest(request))
  def viewLogin(err : String)(implicit request : Request[_]) : templates.Html = 
    viewLogin(Some(err))
  def needLogin(implicit request : SiteRequest[_]) =
    Forbidden(viewLogin(Some(Messages("login.noCookie"))))

  def view = Action { implicit request => 
    Ok(viewLogin())
  }
  
  def post = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      form => BadRequest(views.html.login(form)(new AnonRequest(request))),
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
        case Redeemed(info) =>
          DB.withSession { implicit session =>
            val qao = models.Account.byOpenid(info.id)
            (if (username.isEmpty) qao else qao.filter(_.username === username)).firstOption
          }.map { a =>
            Redirect(routes.Entity.view(a.id)).withSession("account" -> a.id.toString)
          }.getOrElse(
            BadRequest(viewLogin(Messages("login.openID.notFound", info.id)))
          ) 
        case Thrown(t) => InternalServerError(viewLogin(t.toString))
      }
    ))
  }

  def logout = Action { implicit request =>
    Ok(viewLogin(Messages("login.logout"))).withNewSession
  }
}
