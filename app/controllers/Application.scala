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

object Application extends Controller {
  
  def ddl = Action {
    Ok(views.html.ddl(Entity.ddl ++ Trust.ddl ++ Account.ddl))
  }

  def test = Action {
    Ok(views.html.test(Account.getUsername("dylan").orNull.access.toString))
  }

  val loginForm = Form(tuple(
    "username" -> text,
    "openid" -> text
  ))
  
  def login = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      blank => Ok(views.html.login(loginForm)),
      { case (username, openid) => AsyncResult(OpenID.redirectURL(openid, routes.Application.openID(username).absoluteURL(), realm = Some("http://" + request.host)).extend1(
        {
          case Redeemed(url) => Redirect(url)
          case Thrown(t) => Ok(views.html.login(loginForm, t.toString))
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
          }.getOrElse {
            Ok(views.html.login(loginForm, Messages("login.openID.notFound", info.id)))
          }
        case Thrown(t) => Ok(views.html.login(loginForm, t.toString))
      }
    ))
  }

  def getAccount(implicit request : Request[_]) : Option[Account] =
    request.session.get("account").flatMap { i => 
      try { Some(i.toInt) }
      catch { case e:java.lang.NumberFormatException => None }
    }.flatMap(Account.getId _)

  def home = Action { implicit request =>
    getAccount.map { a =>
      Ok(views.html.home(a))
    }.getOrElse {
      Ok(views.html.login(loginForm, Messages("login.noCookie")))
    }
  }
}
