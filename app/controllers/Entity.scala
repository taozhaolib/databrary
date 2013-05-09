package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          db.slick.DB
import          db.slick.Config.driver.simple._
import          i18n.Messages
import models._

object Entity extends Controller {

  def viewEntity(a : Option[Account], e : Entity) = 
    Ok(views.html.entity(a, e))

  def view(i : Int) = Action { request =>
    var e = models.Entity.get(i)
    if (e == null)
      NotFound
    else
      viewEntity(AccountAction.getAccount(request), e)
  }

  val accountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))

  def accountFormFill(a : Account) = accountForm.fill((a.email, a.openid.getOrElse("")))

  def authorizeForm(child : Int, parent : Int) : Form[Authorize] = Form(mapping(
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
      Some((t.access.id, t.delegate.id, t.authorized.fold(true)(_ => false), t.expires.map(e => new java.sql.Date(e.getTime))))
    )
  )

  val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  def viewAdmin(acct : Account, entity : Entity)(
    accountForm : Option[Form[(String, String)]] = entity.account.map(accountFormFill(_)),
    authorizeChangeForm : Option[(Entity,Form[Authorize])] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(Entity,Form[Authorize])] = Seq()) = {
    val authorizeChange = authorizeChangeForm.fold(-1)(_._1.id)
    val authorizeForms = entity.authorizeChildren.filter(_.child != authorizeChange).map(t => (t.childEntity, authorizeForm(t.child, t.parent).fill(t))) ++ authorizeChangeForm
    views.html.entityAdmin(acct, entity, accountForm, authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults)
  }
  
  def checkAdmin(i : Int)(act : (AccountRequest[AnyContent], Entity) => Result) = AccountAction { request =>
    if (Authorize.delegate_check(request.account.id, i) < Permission.ADMIN)
      Forbidden
    else
      act(request, models.Entity.get(i))
  }

  def admin(i : Int) = checkAdmin(i) { (request, entity) =>
    Ok(viewAdmin(request.account, entity)()) 
  }

  def change(i : Int) = checkAdmin(i) { (request, entity) =>
    accountForm.bindFromRequest()(request).fold(
      form => BadRequest(viewAdmin(request.account, entity)(accountForm = Some(form))),
      { case (email, openid) =>
        val a = entity.account.get
        a.email = email
        a.openid = if (openid.isEmpty) None else Some(openid)
        a.commit
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeChange(i : Int, child : Int) = checkAdmin(i) { (request, entity) =>
    authorizeForm(child, entity.id).bindFromRequest()(request).fold(
      form => BadRequest(viewAdmin(request.account, entity)(authorizeChangeForm = Some((models.Entity.get(child), form)))),
      authorize => {
        authorize.commit
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def authorizeDelete(i : Int, child : Int) = checkAdmin(i) { (request, entity) =>
    Authorize.delete(child, entity.id)
    Redirect(routes.Entity.admin(entity.id))
  }

  def authorizeSearch(i : Int, which : Boolean) = checkAdmin(i) { (request, entity) =>
    val form = authorizeSearchForm.bindFromRequest()(request)
    form.fold(
      form => BadRequest(viewAdmin(request.account, entity)(authorizeWhich = Some(which), authorizeSearchForm = form)),
      name => {
        val me = entity.id
        val res = DB.withSession { implicit session =>
          models.Entity.byName(name).filter(e => e.id =!= me && e.id.notIn(Authorize.byParent(me).map(_.child)) && e.id.notIn(Authorize.byChild(me).map(_.parent))).take(8).list
        }
        Ok(viewAdmin(request.account, entity)(authorizeWhich = Some(which), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e,authorizeForm(e.id,me)/* TODO: fill expires */))))
      }
    )
  }

  def authorizeAdd(i : Int, which : Boolean, other : Int) = checkAdmin(i) { (request, entity) =>
    (if (which) authorizeForm(i, other) else authorizeForm(other, i)).bindFromRequest()(request).fold(
      form => BadRequest(viewAdmin(request.account, entity)(authorizeWhich = Some(which), authorizeResults = Seq((models.Entity.get(other), form)))),
      authorize => {
        if (which)
          authorize.authorized = None
        authorize.add
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

}
