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
    var e = Identity.get(i)
    if (e eq null)
      NotFound
    else
      Ok(views.html.entity(e))
  }

  private[this] val accountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))
  private[this] def accountFormFill(u : User) = accountForm.fill((u.email, u.openid.getOrElse("")))

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
      if (t.childId == child && t.parentId == parent)
        Some((t.access.id, t.delegate.id, t.authorized.fold(true)(_ => false), t.expires.map(e => new java.sql.Date(e.getTime))))
      else
        None
    )
  )

  private[this] def authorizeFormWhich(me : Identity, other : Int, which : Boolean) =
    if (which)
      authorizeForm(me.id, other)
    else
      authorizeForm(other, me.id)

  private[this] val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewAdmin(entity : Identity)(
    accountForm : Option[Form[(String, String)]] = entity.user.map(accountFormFill(_)),
    authorizeChangeForm : Option[(Identity,Form[Authorize])] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(Identity,Form[Authorize])] = Seq())(
    implicit request : UserRequest[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    val authorizeForms = entity.authorizeChildren(true).filter(t => Some(t.childId) != authorizeChange).map(t => (t.child, authorizeForm(t.childId, t.parentId).fill(t))) ++ authorizeChangeForm
    views.html.entityAdmin(entity, accountForm, authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults)
  }
  
  private[this] def checkAdmin(i : Int)(act : Identity => UserRequest[AnyContent] => Result) = UserAction { implicit request =>
    if (Authorize.delegate_check(request.identity.id, i) < Permission.ADMIN)
      Forbidden
    else
      act(Identity.get(i))(request)
  }

  def admin(i : Int) = checkAdmin(i) { entity => implicit requset =>
    Ok(viewAdmin(entity)()) 
  }

  def change(i : Int) = checkAdmin(i) { entity => implicit request =>
    val u = entity.asInstanceOf[User]
    accountFormFill(u).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(accountForm = Some(form))),
      { case (email, openid) =>
        u.email = email
        u.openid = maybe(openid)
        u.commit
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeChange(i : Int, child : Int) = checkAdmin(i) { entity => implicit request =>
    authorizeForm(child, entity.id).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(authorizeChangeForm = Some((Identity.get(child), form)))),
      authorize => {
        authorize.commit
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeDelete(i : Int, child : Int) = checkAdmin(i) { entity => implicit request =>
    Authorize.delete((child, entity.id))
    Redirect(routes.Entity.admin(entity.id))
  }

  def authorizeSearch(i : Int, which : Boolean) = checkAdmin(i) { entity => implicit request =>
    val form = authorizeSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(entity)(authorizeWhich = Some(which), authorizeSearchForm = form)),
      name => {
        val me = entity.id
        val res = Identity.byName(name).filter(e => e.id =!= me && e.id.notIn(Authorize.byParent(me).map(_.child)) && e.id.notIn(Authorize.byChild(me).map(_.parent))).take(8).list
        Ok(viewAdmin(entity)(authorizeWhich = Some(which), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e,authorizeFormWhich(entity, e.id, which)
            /* TODO: fill expires */))))
      }
    )
  }

  def authorizeAdd(i : Int, which : Boolean, other : Int) = checkAdmin(i) { entity => implicit request =>
    authorizeFormWhich(entity, other, which).bindFromRequest.fold(
      form => BadRequest(viewAdmin(entity)(authorizeWhich = Some(which), authorizeResults = Seq((Identity.get(other), form)))),
      authorize => {
        if (which)
          authorize.authorized = None
        authorize.add
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }
}
