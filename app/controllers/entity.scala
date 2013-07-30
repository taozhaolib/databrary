package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import dbrary._
import util._
import models._

object Entity extends SiteController {

  def view(i : models.Entity.Id) = SiteAction { implicit request =>
    models.Entity.get(i).fold(NotFound : Result)(
      e => Ok(views.html.entity(e, e.delegated)))
  }

  type EntityForm = Form[(String, Option[Orcid])]
  private[this] val entityForm : EntityForm = Form(tuple(
    "name" -> nonEmptyText,
    "orcid" -> text(0,20).transform[Option[Orcid]](maybe(_).map(Orcid.apply _), _.fold("")(_.toString))
      .verifying("invalid ORCID iD", _.fold(true)(_.valid))
  ))
  private[this] def entityFormFill(e : models.Entity) : EntityForm = entityForm.fill((e.name, e.orcid))

  type AccountForm = Form[(String, String, String)]
  private[this] val accountForm : AccountForm = Form(tuple(
    "email" -> email,
    "openid" -> text(0,256),
    "timezone" -> text(0,32)
  ))
  private[this] def accountFormFill(u : Account) : AccountForm = accountForm.fill((u.email, u.openid.getOrElse(""), u.timezone.getOrElse("")))

  private[this] def authorizeForm(child : models.Entity.Id, parent : models.Entity.Id, which : Boolean = false) : Form[Authorize] = Form(
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

  private[this] def authorizeFormWhich(me : models.Entity, other : models.Entity.Id, which : Boolean) =
    if (which)
      authorizeForm(me.id, other, which)
    else
      authorizeForm(other, me.id, which)

  private[this] val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewAdmin(entity : models.Entity)(
    entityForm : EntityForm = entityFormFill(entity),
    accountForm : Option[AccountForm] = cast[models.Account](entity).map(accountFormFill(_)),
    authorizeChangeForm : Option[(models.Entity,Form[Authorize])] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(models.Entity,Form[Authorize])] = Seq())(
    implicit request : UserRequest[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    val authorizeForms = entity.authorizeChildren(true).filter(t => Some(t.childId) != authorizeChange).map(t => (t.child, authorizeForm(t.childId, t.parentId).fill(t))) ++ authorizeChangeForm
    views.html.entityAdmin(entity, entityForm, accountForm.filter(_ => entity.equals(request.identity)), authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults)
  }
  
  private[this] def checkAdmin(i : models.Entity.Id, delegate : Boolean = true)(act : models.Entity => UserRequest[AnyContent] => Result) = UserAction { implicit request =>
    if (request.identity.id != i && (!delegate || request.identity.delegatedBy(i) < Permission.ADMIN))
      Forbidden
    else
      act(models.Entity.get(i).get)(request)
  }

  def admin(i : models.Entity.Id) = checkAdmin(i) { entity => implicit requset =>
    Ok(viewAdmin(entity)()) 
  }

  def change(i : models.Entity.Id) = checkAdmin(i) { entity => implicit request =>
    entityFormFill(entity).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(entityForm = form)),
      { case (name, orcid) =>
        entity.change(name = name, orcid = orcid)
        Redirect(entity.pageURL)
      }
    )
  }

  def changeAccount(i : models.Entity.Id) = checkAdmin(i, false) { entity => implicit request =>
    val u = entity.asInstanceOf[Account]
    accountFormFill(u).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(accountForm = Some(form))),
      { case (email, openid, timezone) =>
        u.changeAccount(email = email, openid = maybe(openid), timezone = maybe(timezone))
        Redirect(entity.pageURL)
      }
    )
  }

  def authorizeChange(i : models.Entity.Id, child : models.Entity.Id) = checkAdmin(i) { entity => implicit request =>
    authorizeForm(child, entity.id).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(authorizeChangeForm = Some((models.Entity.get(child).get, form)))),
      authorize => {
        authorize.set
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeDelete(i : models.Entity.Id, child : models.Entity.Id) = checkAdmin(i) { entity => implicit request =>
    models.Authorize.delete(child, entity.id)
    Redirect(routes.Entity.admin(entity.id))
  }

  def authorizeSearch(i : models.Entity.Id, which : Boolean) = checkAdmin(i) { entity => implicit request =>
    val form = authorizeSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(entity)(authorizeWhich = Some(which), authorizeSearchForm = form)),
      name => {
        val res = models.Entity.searchForAuthorize(name, entity.id)
        Ok(viewAdmin(entity)(authorizeWhich = Some(which), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e,authorizeFormWhich(entity, e.id, which)
            /* TODO: fill expires */))))
      }
    )
  }

  def authorizeAdd(i : models.Entity.Id, which : Boolean, other : models.Entity.Id) = checkAdmin(i) { entity => implicit request =>
    authorizeFormWhich(entity, other, which).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(authorizeWhich = Some(which), authorizeResults = Seq((models.Entity.get(other).get, form)))),
      authorize => {
        authorize.set
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }
}
