package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import org.mindrot.jbcrypt.BCrypt
import macros._
import dbrary._
import site._
import models._

object Party extends SiteController {
  type Request[A] = RequestObject[SiteParty]#Site[A]

  /** ActionBuilder for party-targeted actions.
    * @param i target party id, defaulting to current user (site.identity)
    * @param p permission needed, or None if delegation is not allowed (must be self)
    */
  private[controllers] def action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    optionZip(i, p).fold[ActionFunction[SiteRequest.Auth,Request]] {
      new ActionRefiner[SiteRequest.Auth,Request] {
        protected def refine[A](request : SiteRequest.Auth[A]) =
          if (i.fold(true)(_.equals(request.identity.id)))
            request.identity.perSite(request).map { p =>
              Right(request.withObj(p))
            }
          else
            macros.Async(Left(Forbidden))
      }
    } { case (i, p) =>
      RequestObject.check[SiteParty](models.SiteParty.get(i)(_), p)
    }

  private[controllers] def Action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    SiteAction.auth ~> action(i, p)

  def view(i : models.Party.Id) = Action(Some(i), Some(Permission.NONE)).async { implicit request =>
    val party = request.obj.party
    if (isAjax) AOk(party.json.obj)
    else for {
      parents <- party.authorizeParents()
      children <- party.authorizeChildren()
      vols <- party.volumeAccess
      fund <- party.funding
      comments <- party.account.fold[Future[Seq[Comment]]](macros.Async(Nil))(_.comments)
    } yield (Ok(views.html.party.view(parents, children, vols, fund, comments)))
  }

  private def adminAccount(implicit request : Request[_]) : Option[Account] =
    request.obj.party.account.filter(_.id.equals(request.identity.id) || request.superuser)

  type PasswordMapping = Mapping[Option[String]]
  val passwordMapping : PasswordMapping = 
    tuple(
      "once" -> optional(text(7)),
      "again" -> text
    ).verifying(Messages("password.again"), pa => pa._1.fold(true)(_ == pa._2))
      .transform[Option[String]](
        _._1.map(BCrypt.hashpw(_, BCrypt.gensalt)),
        _.map(_ => "") -> ""
      )

  type EditForm = Form[(String, Option[Orcid], Option[(String, String, Option[String], String)])]
  private[this] def formFill(implicit request : Request[_]) : EditForm = {
    val e = request.obj.party
    val acct = adminAccount
    Form(tuple(
      "name" -> nonEmptyText,
      "orcid" -> text(0,20).transform[Option[Orcid]](Maybe(_).opt.map(Orcid.apply _), _.fold("")(_.toString))
        .verifying(Messages("orcid.invalid"), _.fold(true)(_.valid)),
      "" -> MaybeMapping(acct.map(_ => tuple(
        "auth" -> text,
        "email" -> email,
        "password" -> passwordMapping,
        "openid" -> text(0,256)
      )))
    )).fill((e.name, e.orcid, acct.map(a => ("", a.email, None, a.openid.getOrElse("")))))
  }

  def formForAccount(form : EditForm)(implicit request : Request[_]) =
    form.value.fold[Option[Any]](adminAccount)(_._3).isDefined

  type AuthorizeForm = Form[(Permission.Value, Permission.Value, Boolean, Option[Date])]
  private val authorizeForm : AuthorizeForm = Form(
    tuple(
      "access" -> Field.enum(Permission),
      "delegate" -> Field.enum(Permission),
      "pending" -> boolean,
      "expires" -> optional(jodaLocalDate)
    )
  )

  private def authorizeFormFill(auth : Authorize, apply : Boolean = false) : AuthorizeForm =
    authorizeForm.fill((auth.access, auth.delegate, auth.authorized.isEmpty, auth.expires.map(_.toLocalDate)))

  private[this] val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewEdit(editForm : Option[EditForm] = None)(implicit request : Request[_]) =
    views.html.party.edit(editForm.getOrElse(formFill))

  private[this] def viewAdmin(
    status : Status,
    authorizeChangeForm : Option[(models.Party,AuthorizeForm)] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(models.Party,AuthorizeForm)] = Seq())(
    implicit request : Request[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    request.obj.party.authorizeChildren(true).flatMap { children =>
    request.obj.party.authorizeParents(true).map { parents =>
      val authorizeForms = children
        .filter(t => authorizeChange.fold(true)(_.equals(t.childId)))
        .map(t => (t.child, authorizeFormFill(t))) ++
        authorizeChangeForm
      status(views.html.party.authorize(parents, authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults))
    }}
  }
  
  private def AdminAction(i : models.Party.Id, delegate : Boolean = true) =
    Action(Some(i), if (delegate) Some(Permission.ADMIN) else None)

  def edit(i : models.Party.Id) = AdminAction(i) { implicit request =>
    Ok(viewEdit())
  }

  def change(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    val form = formFill.bindFromRequest
    val acct = adminAccount
    form.fold(
      form => ABadRequest(viewEdit(editForm = Some(form))),
      { case (_, _, Some((cur, _, _, _))) if !acct.fold(false)(a => BCrypt.checkpw(cur, a.password)) =>
          ABadRequest(viewEdit(editForm = Some(form.withError("cur_password", "password.incorrect"))))
        case (name, orcid, accts) => for {
          _ <- request.obj.party.change(name = name, orcid = orcid)
          _ <- macros.Async.map[(String, String, Option[String], String), Boolean](accts, { case (_, email, password, openid) =>
            acct.get.changeAccount(
              email = email,
              password = password.getOrElse(acct.get.password),
              openid = Maybe(openid).opt)
          })
        } yield (Redirect(request.obj.pageURL))
      }
    )
  }

  def admin(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    viewAdmin(Ok)
  }

  private final val maxExpiration = org.joda.time.Years.years(1)

  def authorizeChange(id : models.Party.Id, childId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(childId).flatMap(_.fold(ANotFound) { child =>
    val form = authorizeForm.bindFromRequest
    form.fold(
      form => viewAdmin(BadRequest, authorizeChangeForm = Some((child, form))),
      { case (access, delegate, pending, expires) =>
        val (exp, expok) = if (request.superuser) (expires, true)
          else {
            val maxexp = (new Date).plus(maxExpiration)
            val exp = expires.getOrElse(maxexp)
            (Some(exp), exp.isAfter(maxexp))
          }
        if (!expok)
          viewAdmin(BadRequest, authorizeChangeForm = Some((child, form.withError("expires", "error.max", maxExpiration))))
        else
          Authorize.set(childId, id, access, delegate, if (pending) None else Some(new Timestamp), exp.map(_.toDateTimeAtStartOfDay)).map { _ =>
            Redirect(routes.Party.admin(id))
          }
      }
    )
    })
  }

  def authorizeDelete(id : models.Party.Id, child : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Authorize.delete(child, id).map { _ =>
      Redirect(routes.Party.admin(id))
    }
  }

  def authorizeSearch(id : models.Party.Id, apply : Boolean) = AdminAction(id).async { implicit request =>
    val form = authorizeSearchForm.bindFromRequest
    form.fold(
      form => viewAdmin(BadRequest, authorizeWhich = Some(apply), authorizeSearchForm = form),
      name =>
        models.Party.searchForAuthorize(name, request.obj.party).flatMap { res =>
        viewAdmin(Ok, authorizeWhich = Some(apply), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e, authorizeForm.fill(
            if (apply) (Permission.NONE, Permission.NONE, true, None)
            else (Permission.NONE, Permission.NONE, false, Some((new Date).plus(maxExpiration)))))))
        }
    )
  }

  def authorizeApply(id : models.Party.Id, parentId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(parentId).flatMap(_.fold(ANotFound) { parent =>
    authorizeForm.bindFromRequest.fold(
      form => viewAdmin(BadRequest, authorizeWhich = Some(true), authorizeResults = Seq((parent, form))),
      { case (access, delegate, _, expires) =>
        Authorize.set(id, parentId, access, delegate, None, expires.map(_.toDateTimeAtStartOfDay)).map { _ =>
          Redirect(routes.Party.admin(id))
        }
      }
    )})
  }
}
