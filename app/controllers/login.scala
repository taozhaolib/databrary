package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import          libs.openid._
import          libs.concurrent._
import                          Execution.Implicits.defaultContext
import          i18n.Messages
import scala.concurrent.Future
import java.net.URL
import org.mindrot.jbcrypt.BCrypt
import macros._
import macros.async._
import dbrary._
import site._
import models._

private[controllers] sealed class LoginController extends SiteController {

  private[controllers] def login(a : Account)(implicit request : SiteRequest[_]) : Future[Result] = {
    Audit.actionFor(Audit.Action.open, a.id, Inet(request.remoteAddress))
    SessionToken.create(a).map { token =>
      (if (request.isApi) Ok((new SiteRequest.Auth(request, token)).json)
      else Redirect(routes.PartyHtml.profile(Some(false))))
        .withSession("session" -> token.id)
    }
  }

  def post = SiteAction.async { implicit request =>
    val form = new LoginController.LoginForm()._bind
    form.email.get.flatMapAsync(Account.getEmail _).flatMap { acct =>
      def bad =
        acct.foreachAsync(a => Audit.actionFor(Audit.Action.attempt, a.id, Inet(request.remoteAddress)).execute).flatMap { _ =>
          form.withGlobalError("login.bad").Bad
        }
      if (form.password.get.nonEmpty) {
        acct.mapAsync(_.recentAttempts).flatMap { attempts =>
          if (attempts.exists(_ > 4))
            form.withGlobalError("login.throttled").Bad
          else
            acct.filter(a => !a.password.isEmpty && BCrypt.checkpw(form.password.get, a.password)).fold(bad)(login)
        }
      } else form.openid.get.fold(bad) { openid =>
        OpenID.redirectURL(openid.toString, routes.LoginHtml.openID(form.email.get.getOrElse("")).absoluteURL(Play.isProd), realm = Some("http://" + request.host))
          .map(Redirect(_))
          .recover { case e : OpenIDError => InternalServerError(LoginHtml.viewLogin(e.toString)) }
      }
    }
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
    (if (request.isApi) Ok((new SiteRequest.Anon(request)).json)
    else Redirect(routes.SiteHtml.start(Some(false))))
      .withNewSession
  }

  private final val superuserTime : Long = 60*60*1000
  def superuserOn = SiteAction.rootAccess() { implicit request =>
    if (!new LoginController.SuperuserForm()._bind._authorized)
      throw ForbiddenException
    val expires = System.currentTimeMillis + superuserTime
    Audit.action(Audit.Action.superuser)
    (if (request.isApi) Ok(request.json - "superuser" + ('superuser -> true))
    else Redirect(request.headers.get(REFERER).getOrElse(routes.SiteHtml.start(Some(false)).url)))
      .withSession(request.session + ("superuser" -> expires.toString))
  }

  def superuserOff = SiteAction { implicit request =>
    (if (request.isApi) Ok(request.json - "superuser" + ('superuser -> false))
    else Redirect(request.headers.get(REFERER).getOrElse(routes.SiteHtml.start(Some(false)).url)))
      .withSession(request.session - "superuser")
  }

  def register =
    SiteAction.async { implicit request =>
      val form = new LoginController.RegistrationForm()._bind
      for {
        e <- Account.getEmail(form.email.get)
        a <- e getOrElseAsync {
          Party.create(
            sortname = form.sortname.get,
            prename = form.prename.get,
            affiliation = form.affiliation.get)
          .flatMap(Account.create(_, email = form.email.get))
        }
        _ <- controllers.TokenController.newPassword(Right(a), "register")
        _ = Mail.investigator(a.party)
      } yield (Ok(Messages("register.sent.instructions", a.email)))
    }
}

object LoginController extends LoginController {
  private[controllers] def needed(message : String)(implicit request : SiteRequest[_]) : Result = {
    val msg = Messages(message)
    if (request.isApi) Forbidden(msg)
    else Forbidden(LoginHtml.viewLogin(msg))
  }

  final class LoginForm(implicit request : SiteRequest[_])
    extends HtmlForm[LoginForm](
      routes.LoginHtml.post,
      views.html.party.login(_))
    with NoCsrfForm {
    val email = Field(Forms.optional(Forms.email))
    val password = Field(Forms.default(Forms.text, ""))
    val openid = Field(Forms.optional(Forms.of[URL]))
    _mapping.verifying("login.bad", self =>
      (self.email.get.isDefined && self.password.get.nonEmpty) || self.openid.get.nonEmpty)
    private[controllers] def _fill(em : Option[String], op : Option[URL] = None) : this.type = {
      email.fill(em)
      openid.fill(op)
      this
    }
  }

  object LogoutForm extends StructForm(routes.LoginHtml.logout)

  final class RegistrationForm(implicit request : SiteRequest[_])
    extends HtmlForm[RegistrationForm](
      routes.LoginHtml.register,
      views.html.party.register(_)) {
    val sortname = Field(Mappings.nonEmptyText)
    val prename = Field(Mappings.maybeText)
    val email = Field(Forms.email)
    val affiliation = Field(Mappings.maybeText)
    val agreement = Field(Forms.checked("agreement.required"))
  }

  trait AuthForm extends StructForm {
    protected def request : AuthSite
    val auth = Field(Forms.default(Forms.text, "").verifying("password.incorrect",
      s => s.isEmpty || BCrypt.checkpw(s, request.account.password))).fill("")
    def _authorized = !hasErrors && (auth.get.nonEmpty || !Play.isProd)
  }

  trait PasswordChangeForm extends StructForm {
    protected val passwordRequired = false
    private[this] final def passwordBaseMapping : Mapping[String] =
      Forms.text(7)
    protected def passwordOnceMapping : Mapping[Option[String]] =
      if (passwordRequired) Mappings.some(passwordBaseMapping)
      else Forms.optional(passwordBaseMapping)
    protected def passwordInputMapping : Mapping[Option[String]] =
      Forms.tuple(
        "once" -> passwordOnceMapping,
        "again" -> passwordOnceMapping
      ).verifying("password.match", pa => pa._1 == pa._2)
      .transform[Option[String]](_._1, p => (p, p))
    protected final def passwordMapping : Mapping[Option[String]] =
      passwordInputMapping
      .transform[Option[String]](identity, _.map(_ => ""))
    protected final def passwordField = Field(passwordMapping)
    val password : Field[Option[String]]

    def cryptPassword : Option[String] =
      password.get.map(BCrypt.hashpw(_, BCrypt.gensalt))
    def checkPassword(account : Account) {
      password.get.flatMap(
        media.Passwd.check(_, account.email, account.party.name))
      .foreach { e =>
        password.withError("password.check", e)
      }
    }
  }

  final class SuperuserForm(implicit protected val request : SiteRequest.Auth[_])
    extends FormView(routes.LoginHtml.superuserOn)
    with AuthForm {
    def account = request.account
    def _exception(status : Results.Status) = ForbiddenException
  }
}

object LoginHtml extends LoginController with HtmlController {
  import LoginController._

  def viewLogin()(implicit request: SiteRequest[_]) : play.twirl.api.Html =
    views.html.party.login(new LoginForm)
  def viewLogin(err : String, args : Any*)(implicit request: SiteRequest[_]) : play.twirl.api.Html =
    views.html.party.login {
      val form = new LoginForm
      form.withGlobalError(err, args)
    }

  def view(js : Option[Boolean]) = SiteAction.js { implicit request =>
    request.user.fold(Ok(viewLogin()))(u => Found(routes.PartyHtml.profile(Some(false)).url))
  }

  def openID(email : String) = SiteAction.async { implicit request =>
    val em = Maybe(email).opt()
    (for {
      info <- OpenID.verifiedId
      acct <- Account.getOpenid(info.id, em)
      r <- acct.fold {
        new LoginForm()._fill(em, url.parse(info.id)).openid.withError("login.openID.notFound").Bad
      } (login)
    } yield (r))
    .recover { case e : OpenIDError => InternalServerError(viewLogin(e.toString)) }
  }

  def registration(js : Option[Boolean]) =
    SiteAction.js.async { implicit request =>
      if (request.isInstanceOf[AuthSite])
        async(Found((
          if (request.access.site == Permission.NONE)
            routes.PartyHtml.profile(Some(false))
          else
            routes.SiteHtml.start(Some(false))).url))
      else
        new RegistrationForm().Ok
    }
}

object LoginApi extends LoginController with ApiController {
  def get = SiteAction { implicit request =>
    Ok(request.json)
  }
}
