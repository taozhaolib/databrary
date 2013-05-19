package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          db.slick.DB
import          db.slick.Config.driver.simple._
import          i18n.Messages
import models._

object Entity extends SiteController {

  def view(i : Int) = SiteAction { implicit request =>
    var e = models.Entity.get(i)
    if (e eq null)
      NotFound
    else
      Ok(views.html.entity(e))
  }

  private[this] val accountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))
  private[this] def accountFormFill(a : Account) = accountForm.fill((a.email, a.openid.getOrElse("")))

  private[this] def authorizeForm(child : Int, parent : Int) : Form[Authorize] = Form(
    mapping(
      "access" -> number(min=0, max=Permission.maxId-1),
      "delegate" -> number(min=0, max=Permission.maxId-1),
      "pending" -> boolean,
      "expires" -> optional(sqlDate)
    )((access, delegate, pending, expires) => Authorize(
      child, parent, 
      Permission(access), 
      Permission(delegate), 
      if (pending) None else Some(new java.sql.Timestamp(System.currentTimeMillis)),
      expires.map(e => new java.sql.Timestamp(e.getTime))
    ))(t => 
      if (t.child == child && t.parent == parent)
        Some((t.access.id, t.delegate.id, t.authorized.fold(true)(_ => false), t.expires.map(e => new java.sql.Date(e.getTime))))
      else
        None
    )
  )

  private[this] def authorizeFormWhich(me : Entity, other : Int, which : Boolean) =
    if (which)
      authorizeForm(me.id, other)
    else
      authorizeForm(other, me.id)

  private[this] val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewAdmin(entity : Entity, account : Option[Account])(
    accountForm : Option[Form[(String, String)]] = account.map(accountFormFill(_)),
    authorizeChangeForm : Option[(Entity,Form[Authorize])] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(Entity,Form[Authorize])] = Seq())(
    implicit request : AccountRequest[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    val authorizeForms = entity.authorizeChildren(true).filter(t => Some(t.child) != authorizeChange).map(t => (t.childEntity, authorizeForm(t.child, t.parent).fill(t))) ++ authorizeChangeForm
    views.html.entityAdmin(entity, accountForm, authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults)
  }
  
  private[this] def checkAdmin(i : Int)(act : Entity => AccountRequest[AnyContent] => Result) = AccountAction { implicit request =>
    if (Authorize.delegate_check(request.account.id, i) < Permission.ADMIN)
      Forbidden
    else
      act(models.Entity.get(i))(request)
  }

  def admin(i : Int) = checkAdmin(i) { entity => implicit requset =>
    Ok(viewAdmin(entity, entity.account)()) 
  }

  def change(i : Int) = checkAdmin(i) { entity => implicit request =>
    val a = entity.account.get
    accountFormFill(a).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity, Some(a))(accountForm = Some(form))),
      { case (email, openid) =>
        a.email = email
        a.openid = maybe(openid)
        a.commit
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeChange(i : Int, child : Int) = checkAdmin(i) { entity => implicit request =>
    authorizeForm(child, entity.id).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity, entity.account)(authorizeChangeForm = Some((models.Entity.get(child), form)))),
      authorize => {
        authorize.commit
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeDelete(i : Int, child : Int) = checkAdmin(i) { entity => implicit request =>
    Authorize.delete(child, entity.id)
    Redirect(routes.Entity.admin(entity.id))
  }

  def authorizeSearch(i : Int, which : Boolean) = checkAdmin(i) { entity => implicit request =>
    val form = authorizeSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(entity, entity.account)(authorizeWhich = Some(which), authorizeSearchForm = form)),
      name => {
        val me = entity.id
        val res = models.Entity.byName(name).filter(e => e.id =!= me && e.id.notIn(Authorize.byParent(me).map(_.child)) && e.id.notIn(Authorize.byChild(me).map(_.parent))).take(8).list
        Ok(viewAdmin(entity, entity.account)(authorizeWhich = Some(which), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e,authorizeFormWhich(entity, e.id, which)
            /* TODO: fill expires */))))
      }
    )
  }

  def authorizeAdd(i : Int, which : Boolean, other : Int) = checkAdmin(i) { entity => implicit request =>
    authorizeFormWhich(entity, other, which).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity, entity.account)(authorizeWhich = Some(which), authorizeResults = Seq((models.Entity.get(other), form)))),
      authorize => {
        if (which)
          authorize.authorized = None
        authorize.add
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }
}
