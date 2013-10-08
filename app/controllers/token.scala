package controllers

import play.api.mvc._
import play.api.data._
import               Forms._
import play.api.i18n.Messages
import site._
import models._

object Token extends SiteController {

  type PasswordForm = Form[(String, Option[String])]
  val passwordForm = Form(tuple(
    "token" -> text,
    "password" -> Party.passwordMapping.verifying(Messages("error.required"), _.isDefined)
  ))

  def token(token : String) = SiteAction { implicit request =>
    models.LoginToken.get(token).fold[SimpleResult](NotFound) { token =>
      if (!token.valid) {
        token.remove
        Gone
      } else token.party match {
        case a : Account if token.password =>
          Ok(views.html.token.password(a.id, passwordForm.fill((token.token, None))))
        case a : Account =>
          Login.login(a) /* TODO: don't allow password resets through this path */
        case _ => NotImplemented
      }
    }
  }

  def password(a : models.Account.Id) = SiteAction { implicit request =>
    passwordForm.bindFromRequest.fold(
      form => BadRequest(views.html.token.password(a, form)),
      { case (token, password) =>
        models.LoginToken.get(token).filter(t => t.valid && t.password && t.party.id == a)
          .fold[SimpleResult](Forbidden) { token =>
          val acct = token.party.asInstanceOf[models.Account] // hopefully
          password.foreach(p => acct.changeAccount(password = p))
          Login.login(acct)
        }
      }
    )
  }
}
