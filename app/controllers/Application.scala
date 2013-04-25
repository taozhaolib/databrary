package controllers

import play.api._
import          mvc._
import          data._
import               Forms._
import          libs.openid._
import          libs.concurrent._
import                          Execution.Implicits.defaultContext
import models._

object Application extends Controller {
  
  def ddl = Action {
    Ok(views.html.ddl(Entity.ddl ++ Trust.ddl ++ Account.ddl))
  }

  def test = Action {
    Ok(views.html.test(Account.getUsername("dylan").orNull.access.toString))
  }

  val loginForm = Form(single("openid" -> text))
  
  def login = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      blank => Ok(views.html.login(loginForm)),
      { case (openid) => AsyncResult(OpenID.redirectURL(openid, routes.Application.openID.absoluteURL(), realm = Some("http://" + request.host)).extend1(
        {
          case Redeemed(url) => Redirect(url)
          case Thrown(t) => Ok(views.html.login(loginForm, t.toString))
        }
      ))}
    )
  }

  def openID = Action { implicit request =>
    Ok(views.html.login(loginForm))
  }
}
