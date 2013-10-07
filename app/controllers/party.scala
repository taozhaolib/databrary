package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import org.mindrot.jbcrypt.BCrypt
import dbrary._
import util._
import models._

object Party extends SiteController {
  type Request[A] = RequestObject[Party]#T[A]

  def view(i : models.Party.Id) = SiteAction { implicit request =>
    models.Party.get(i).fold(NotFound : Result)(
      e => Ok(views.html.party.view(e)))
  }

  def ajaxView() = SiteAction { implicit request =>
    request.user.fold(NotFound : Result)(
      e => Ok(views.html.modal.profile(request.identity)))
  }

  private def adminAccount(implicit request : Request[_]) =
    cast[models.Account](request.obj).filter(_.equals(request.identity))

  type EditForm = Form[(String, Option[Orcid], Option[(String, Option[String], String, String)])]
  private[this] def formFill(implicit request : Request[_]) : EditForm = {
    val e = request.obj
    val acct = adminAccount
    Form(tuple(
      "name" -> nonEmptyText,
      "orcid" -> text(0,20).transform[Option[Orcid]](maybe(_).map(Orcid.apply _), _.fold("")(_.toString))
        .verifying(Messages("orcid.invalid"), _.fold(true)(_.valid)),
      "" -> MaybeMapping(acct.map(_ => tuple(
        "email" -> email,
        "password" -> optional(text(7)),
        "again" -> text,
        "openid" -> text(0,256)
      ).verifying(Messages("password.again"), f => f._2.fold(true)(_ == f._3))))
    )).fill((e.name, e.orcid, acct.map(a => (a.email, None, "", a.openid.getOrElse("")))))
  }

  def formForAccount(form : EditForm)(implicit request : Request[_]) =
    form.value.fold(adminAccount : Option[Any])(_._3).isDefined

  private[this] def authorizeForm(child : models.Party.Id, parent : models.Party.Id, which : Boolean = false) : Form[Authorize] = Form(
    mapping(
      "access" -> Field.enum(Permission),
      "delegate" -> Field.enum(Permission),
      "pending" -> boolean,
      "expires" -> optional(sqlDate)
    )((access, delegate, pending, expires) => Authorize(
      child, parent, access, delegate, 
      if (pending || which) None else Some(new java.sql.Timestamp(System.currentTimeMillis)),
      expires.map(e => new java.sql.Timestamp(e.getTime))
    ))(t => 
      if (t.childId == child && t.parentId == parent)
        Some((t.access, t.delegate, t.authorized.fold(true)(_ => false), t.expires.map(e => new java.sql.Date(e.getTime))))
      else
        None
    )
  )

  private[this] def authorizeFormWhich(me : models.Party, other : models.Party.Id, which : Boolean) =
    if (which)
      authorizeForm(me.id, other, which)
    else
      authorizeForm(other, me.id, which)

  private[this] val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewEdit(editForm : Option[EditForm] = None)(implicit request : Request[_]) =
    views.html.party.edit(editForm.getOrElse(formFill))

  private[this] def viewAdmin(
    authorizeChangeForm : Option[(models.Party,Form[Authorize])] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(models.Party,Form[Authorize])] = Seq())(
    implicit request : Request[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    val authorizeForms = request.obj.authorizeChildren(true).filter(t => Some(t.childId) != authorizeChange).map(t => (t.child, authorizeForm(t.childId, t.parentId).fill(t))) ++ authorizeChangeForm
    views.html.party.admin(authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults)
  }
  
  private[this] def AdminAction(i : models.Party.Id, delegate : Boolean = true) =
    SiteAction.user ~> new ActionRefiner[UserRequest,Request] {
      protected def refine[A](request : UserRequest[A]) =
        if (request.identity.id != i && (!delegate || request.identity.delegatedBy(i)(request) < Permission.ADMIN))
          simple(Forbidden)
        else
          Right(RequestObject[Party,A](request, models.Party.get(i)(request).get))
    }

  def edit(i : models.Party.Id) = AdminAction(i) { implicit request =>
    Ok(viewEdit())
  }

  def change(i : models.Party.Id) = AdminAction(i) { implicit request =>
    formFill.bindFromRequest.fold(
      form => BadRequest(viewEdit(editForm = Some(form))),
      { case (name, orcid, acct) =>
        request.obj.change(name = name, orcid = orcid)
        acct foreach {
          case (email, password, _, openid) =>
            val acct = request.obj.asInstanceOf[models.Account]
            acct.changeAccount(
              email = email,
              password = password.fold(acct.password)(BCrypt.hashpw(_, BCrypt.gensalt)),
              openid = maybe(openid))
        }
        Redirect(request.obj.pageURL)
      }
    )
  }

  def admin(i : models.Party.Id) = AdminAction(i) { implicit request =>
    Ok(viewAdmin())
  }

  def authorizeChange(id : models.Party.Id, child : models.Party.Id) = AdminAction(id) { implicit request =>
    authorizeForm(child, id).bindFromRequest.fold(
      form => BadRequest(viewAdmin(authorizeChangeForm = Some((models.Party.get(child).get, form)))),
      authorize => {
        authorize.set
        Redirect(routes.Party.admin(id))
      }
    )
  }

  def authorizeDelete(id : models.Party.Id, child : models.Party.Id) = AdminAction(id) { implicit request =>
    models.Authorize.delete(child, id)
    Redirect(routes.Party.admin(id))
  }

  def authorizeSearch(id : models.Party.Id, which : Boolean) = AdminAction(id) { implicit request =>
    val form = authorizeSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(authorizeWhich = Some(which), authorizeSearchForm = form)),
      name => {
        val res = models.Party.searchForAuthorize(name, id)
        Ok(viewAdmin(authorizeWhich = Some(which), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e,authorizeFormWhich(request.obj, e.id, which)
            /* TODO: fill expires */))))
      }
    )
  }

  def authorizeAdd(id : models.Party.Id, which : Boolean, other : models.Party.Id) = AdminAction(id) { implicit request =>
    authorizeFormWhich(request.obj, other, which).bindFromRequest.fold(
      form => BadRequest(viewAdmin(authorizeWhich = Some(which), authorizeResults = Seq((models.Party.get(other).get, form)))),
      authorize => {
        authorize.set
        Redirect(routes.Party.admin(id))
      }
    )
  }
}
