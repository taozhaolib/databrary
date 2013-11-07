package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import               Forms._
import play.api.i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import site._
import models._

object Token extends SiteController {

  type PasswordForm = Form[(String, Option[String])]
  val passwordForm = Form(tuple(
    "token" -> text,
    "password" -> Party.passwordMapping.verifying(Messages("error.required"), _.isDefined)
  ))

  def token(token : String) = SiteAction.async { implicit request =>
    models.LoginToken.get(token).map(_.fold[SimpleResult](
      NotFound
    ) { token =>
      if (!token.valid) {
        token.remove
        Gone
      } else if (token.password)
        Ok(views.html.token.password(token.accountId, passwordForm.fill((token.token, None))))
      else {
        token.remove
        Login.login(token.account) /* TODO: don't allow password resets through this path */
      }
    })
  }

  def password(a : models.Account.Id) = SiteAction.async { implicit request =>
    passwordForm.bindFromRequest.fold(
      form => ABadRequest(views.html.token.password(a, form)),
      { case (token, password) =>
        models.LoginToken.get(token).flatMap(_
          .filter(t => t.valid && t.password && t.accountId == a)
          .fold[Future[SimpleResult]](AForbidden) { token =>
            password.fold(macros.Async(false))(p => token.account.changeAccount(password = p)).map { _ =>
              Login.login(token.account)
            }
          }
        )
      }
    )
  }

  private lazy val mailer = Play.current.plugin[com.typesafe.plugin.MailerPlugin]

  val issuePasswordForm = Form("email" -> email)

  def getPassword = SiteAction { implicit request =>
    mailer.fold[SimpleResult](ServiceUnavailable) { mailer =>
      Ok(views.html.token.getPassword(issuePasswordForm))
    }
  }

  def issuePassword = SiteAction.async { implicit request =>
    issuePasswordForm.bindFromRequest.fold(
      form => ABadRequest(views.html.token.getPassword(form)),
      email => mailer.fold[Future[SimpleResult]](macros.Async(ServiceUnavailable)) { mailer =>
        implicit val defaultContext = context.process
        for {
          acct <- Account.getEmail(email)
          acct <- macros.Async.filter[Account](acct, _.access.map(_ < Permission.ADMIN))
          token <- macros.Async.map[Account,Token](acct, LoginToken.create(_, true))
        } yield {
          val mail = mailer.email
          mail.setSubject(Messages("token.password.subject"))
          mail.setRecipient(email)
          mail.setFrom("Databrary <help@databrary.org>")
          token.fold {
            mail.send(Messages("token.password.none"))
          } { token =>
            mail.send(Messages("token.password.body", token.redeemURL.absoluteURL()))
          }
          Ok("sent")
        }
      }
    )
  }
}
