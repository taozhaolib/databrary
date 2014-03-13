package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import site._
import models._

private[controllers] sealed class TokenController extends SiteController {
  def password(a : models.Account.Id) = SiteAction.async { implicit request =>
    val form = new TokenController.PasswordForm(a)._bind
    models.LoginToken.get(form.token.get).flatMap(_
      .filter(t => t.valid && form.auth.get.equals(t.auth) && t.password && t.accountId === a)
      .fold[Future[SimpleResult]](ForbiddenException.result) { token =>
	form.password.get.fold(macros.Async(false))(p => token.account.change(password = Some(p))).flatMap { _ =>
	  LoginController.login(token.account)
	}
      }
    )
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
}

object TokenController extends TokenController {
  final class PasswordForm(accountId : Account.Id)(implicit request : SiteRequest[_])
    extends HtmlForm[PasswordForm](
      routes.TokenHtml.password(accountId),
      views.html.token.password(_)) {
    val token = Field(Forms.text)
    val auth = Field(Forms.text)
    val password = Field(PartyHtml.passwordMapping.verifying("error.required", _.isDefined)).fill(None)
    private[controllers] def _fill(t : LoginToken) : this.type = {
      assert(accountId === t.accountId)
      assert(t.password)
      token.fill(t.id)
      auth.fill(t.auth)
      this
    }
  }
}

object TokenHtml extends TokenController with HtmlController {
  def token(token : String, auth : String) = SiteAction.async { implicit request =>
    models.LoginToken.get(token).flatMap(_.fold(
      ANotFound
    ) { token =>
      if (!token.valid) {
        token.remove
        macros.Async(Gone)
      } else if (!auth.equals(token.auth)) {
        throw ForbiddenException
      } else if (token.password)
	new TokenController.PasswordForm(token.accountId)._fill(token).Ok
      else {
        token.remove
        LoginController.login(token.account)
      }
    })
  }

  final class IssuePasswordForm(implicit request : SiteRequest[_])
    extends HtmlForm[IssuePasswordForm](
      routes.TokenHtml.issuePassword,
      views.html.token.getPassword(_)) {
    val email = Field(Forms.email)
  }

  def getPassword = SiteAction.async { implicit request =>
    Mail.check
    new IssuePasswordForm().Ok
  }

  def issuePassword = SiteAction.async { implicit request =>
    val form = new IssuePasswordForm()._bind
    for {
      acct <- Account.getEmail(form.email.get)
      acct <- macros.Async.filter[Account](acct, _.party.access.map(_.direct < Permission.ADMIN))
      _ <- newPassword(acct.toRight(form.email.get))
    } yield (Ok("sent"))
  }
}

object TokenApi extends TokenController with ApiController {
}
