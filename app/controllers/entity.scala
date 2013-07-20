package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Entity extends SiteController {

  def view(i : Identity.Id) = SiteAction { implicit request =>
    Identity.get(i).fold(NotFound : Result)(
      e => Ok(views.html.entity(e, e.delegated)))
  }

  type EntityForm = Form[(String, Option[Orcid])]
  private[this] val entityForm : EntityForm = Form(tuple(
    "name" -> nonEmptyText,
    "orcid" -> text.transform[Option[Orcid]](maybe(_).map(Orcid.apply _), _.fold("")(_.toString))
      .verifying("invalid ORCID iD", _.fold(true)(_.valid))
  ))
  private[this] def entityFormFill(e : Identity) : EntityForm = entityForm.fill((e.name, e.orcid))

  type AccountForm = Form[(String, String)]
  private[this] val accountForm : AccountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))
  private[this] def accountFormFill(u : User) : AccountForm = accountForm.fill((u.email, u.openid.getOrElse("")))

  private[this] def authorizeForm(child : Identity.Id, parent : Identity.Id, which : Boolean = false) : Form[Authorize] = Form(
    mapping(
      "access" -> number(min=0, max=Permission.maxId-1),
      "delegate" -> number(min=0, max=Permission.maxId-1),
      "pending" -> boolean,
      "expires" -> optional(sqlDate)
    )((access, delegate, pending, expires) => Authorize(
      child, parent, 
      Permission(access), 
      Permission(delegate), 
      if (pending || which) None else Some(new java.sql.Timestamp(System.currentTimeMillis)),
      expires.map(e => new java.sql.Timestamp(e.getTime))
    ))(t => 
      if (t.childId == child && t.parentId == parent)
        Some((t.access.id, t.delegate.id, t.authorized.fold(true)(_ => false), t.expires.map(e => new java.sql.Date(e.getTime))))
      else
        None
    )
  )

  private[this] def authorizeFormWhich(me : Identity, other : Identity.Id, which : Boolean) =
    if (which)
      authorizeForm(me.id, other, which)
    else
      authorizeForm(other, me.id, which)

  private[this] val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewAdmin(entity : Identity)(
    entityForm : EntityForm = entityFormFill(entity),
    accountForm : Option[AccountForm] = entity.user.map(accountFormFill(_)),
    authorizeChangeForm : Option[(Identity,Form[Authorize])] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(Identity,Form[Authorize])] = Seq())(
    implicit request : UserRequest[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    val authorizeForms = entity.authorizeChildren(true).filter(t => Some(t.childId) != authorizeChange).map(t => (t.child, authorizeForm(t.childId, t.parentId).fill(t))) ++ authorizeChangeForm
    views.html.entityAdmin(entity, entityForm, accountForm, authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults)
  }
  
  private[this] def checkAdmin(i : Identity.Id)(act : Identity => UserRequest[AnyContent] => Result) = UserAction { implicit request =>
    if (request.identity.delegatedBy(i) < Permission.ADMIN)
      Forbidden
    else
      act(Identity.get(i).get)(request)
  }

  def admin(i : Identity.Id) = checkAdmin(i) { entity => implicit requset =>
    Ok(viewAdmin(entity)()) 
  }

  def change(i : Identity.Id) = checkAdmin(i) { entity => implicit request =>
    entityFormFill(entity).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(entityForm = form)),
      { case (name, orcid) =>
        entity.changeEntity(name = name, orcid = orcid)
        Redirect(routes.Entity.view(entity.id))
      }
    )
  }

  def changeAccount(i : Identity.Id) = checkAdmin(i) { entity => implicit request =>
    val u = entity.user.get
    accountFormFill(u).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(accountForm = Some(form))),
      { case (email, openid) =>
        u.changeAccount(email = email, openid = maybe(openid))
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeChange(i : Identity.Id, child : Identity.Id) = checkAdmin(i) { entity => implicit request =>
    authorizeForm(child, entity.id).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(authorizeChangeForm = Some((Identity.get(child).get, form)))),
      authorize => {
        authorize.set
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeDelete(i : Identity.Id, child : Identity.Id) = checkAdmin(i) { entity => implicit request =>
    models.Authorize.delete(child, entity.id)
    Redirect(routes.Entity.admin(entity.id))
  }

  def authorizeSearch(i : Identity.Id, which : Boolean) = checkAdmin(i) { entity => implicit request =>
    val form = authorizeSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(entity)(authorizeWhich = Some(which), authorizeSearchForm = form)),
      name => {
        val res = Identity.searchForAuthorize(name, entity.id)
        Ok(viewAdmin(entity)(authorizeWhich = Some(which), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e,authorizeFormWhich(entity, e.id, which)
            /* TODO: fill expires */))))
      }
    )
  }

  def authorizeAdd(i : Identity.Id, which : Boolean, other : Identity.Id) = checkAdmin(i) { entity => implicit request =>
    authorizeFormWhich(entity, other, which).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(authorizeWhich = Some(which), authorizeResults = Seq((Identity.get(other).get, form)))),
      authorize => {
        authorize.set
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }
}
