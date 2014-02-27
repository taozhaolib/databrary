package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import site._
import models._

object TokenController extends SiteController {

  final class PasswordForm(accountId : Account.Id)(implicit request : SiteRequest[_])
    extends HtmlForm[PasswordForm](
      routes.TokenController.password(accountId),
      views.html.token.password(_)) {
    val token = Field(Forms.text)
    val password = Field(PartyHtml.passwordMapping.verifying("error.required", _.isDefined)).fill(None)
    private[TokenController] def _fill(t : LoginToken) : this.type = {
      assert(accountId === t.accountId)
      assert(t.password)
      token.fill(t.id)
      this
    }
  }

  def token(token : String) = SiteAction.async { implicit request =>
    models.LoginToken.get(token).flatMap(_.fold(
      ANotFound
    ) { token =>
      if (!token.valid) {
        token.remove
        macros.Async(Gone)
      } else if (token.password)
	new PasswordForm(token.accountId)._fill(token).Ok
      else {
        token.remove
        LoginController.login(token.account)
      }
    })
  }

  def password(a : models.Account.Id) = SiteAction.async { implicit request =>
    val form = new PasswordForm(a)._bind
    models.LoginToken.get(form.token.get).flatMap(_
      .filter(t => t.valid && t.password && t.accountId === a)
      .fold[Future[SimpleResult]](ForbiddenException.result) { token =>
	form.password.get.fold(macros.Async(false))(p => token.account.change(password = Some(p))).flatMap { _ =>
	  LoginController.login(token.account)
	}
      }
    )
  }

  final class IssuePasswordForm(implicit request : SiteRequest[_])
    extends HtmlForm[IssuePasswordForm](
      routes.TokenController.issuePassword,
      views.html.token.getPassword(_)) {
    val email = Field(Forms.email)
  }

  def getPassword = SiteAction.async { implicit request =>
    Mail.check
    new IssuePasswordForm().Ok
  }

  private[controllers] def newPassword(targ : Either[String,Account], msg : String = "password")(implicit request : SiteRequest[_]) : Future[Option[LoginToken]] =
    for {
      token <- macros.Async.map[Account,LoginToken](targ.right.toOption, LoginToken.create(_, true))
      _ <- Mail.send(
	to = targ.fold(identity, _.email),
	subject = Messages("mail." + msg + ".subject"),
	body = token.fold {
	  Messages("mail." + msg + ".none")
	} { token =>
	  Messages("mail." + msg + ".body", token.redeemURL.absoluteURL(true))
	}
      )
    } yield (token)

  def issuePassword = SiteAction.async { implicit request =>
    val form = new IssuePasswordForm()._bind
    for {
      acct <- Account.getEmail(form.email.get)
      acct <- macros.Async.filter[Account](acct, _.party.access.map(_.direct < Permission.ADMIN))
      _ <- newPassword(acct.toRight(form.email.get))
    } yield (Ok("sent"))
  }
}
