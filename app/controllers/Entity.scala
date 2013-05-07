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

  def trustForm(child : Int, parent : Int) : Form[Trust] = Form(mapping(
      "access" -> number(min=0, max=SitePermission.maxId-1),
      "delegate" -> number(min=0, max=UserPermission.maxId-1),
      "pending" -> boolean,
      "expires" -> optional(sqlDate)
    )((access, delegate, pending, expires) => Trust(
      child, parent, 
      SitePermission(access), 
      UserPermission(delegate), 
      if (pending) None else Some(new java.sql.Timestamp(System.currentTimeMillis)),
      expires.map(e => new java.sql.Timestamp(e.getTime))
    ))(t => 
      Some((t.access.id, t.delegate.id, t.authorized.fold(true)(_ => false), t.expires.map(e => new java.sql.Date(e.getTime))))
    )
  )

  val trustSearchForm = Form(
    "name" -> nonEmptyText
  )

  def viewAdmin(acct : Account, entity : Entity)(
    accountForm : Option[Form[(String, String)]] = entity.account.map(accountFormFill(_)),
    trustChangeForm : Option[(Entity,Form[Trust])] = None,
    trustSearchForm : Form[String] = trustSearchForm,
    trustResults : Seq[(Entity,Form[Trust])] = Seq()) = {
    val trustChange = trustChangeForm.fold(-1)(_._1.id)
    val trustForms = entity.trustChildren.filter(_.child != trustChange).map(t => (t.childEntity, trustForm(t.child, t.parent).fill(t))) ++ trustChangeForm
    views.html.entityAdmin(acct, entity, accountForm, trustForms, trustSearchForm, trustResults)
  }
  
  def checkAdmin(i : Int)(act : (AccountRequest[AnyContent], Entity) => Result) = AccountAction { request =>
    if (Trust.delegate_check(request.account.id, i) < UserPermission.ADMIN)
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

  def trustChange(i : Int, child : Int) = checkAdmin(i) { (request, entity) =>
    trustForm(child, entity.id).bindFromRequest()(request).fold(
      form => BadRequest(viewAdmin(request.account, entity)(trustChangeForm = Some((models.Entity.get(child), form)))),
      trust => {
        trust.commit
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

  def trustDelete(i : Int, child : Int) = checkAdmin(i) { (request, entity) =>
    Trust.delete(child, entity.id)
    Redirect(routes.Entity.admin(entity.id))
  }

  def trustSearch(i : Int) = checkAdmin(i) { (request, entity) =>
    val form = trustSearchForm.bindFromRequest()(request)
    form.fold(
      form => BadRequest(viewAdmin(request.account, entity)(trustSearchForm = form)),
      name => {
        val me = entity.id
        val res = DB.withSession { implicit session =>
          models.Entity.byName(name).filter(e => e.id =!= me && e.id.notIn(Trust.byParent(me).map(_.child))).take(8).list
        }
        Ok(viewAdmin(request.account, entity)(trustSearchForm = form, 
          trustResults = res.map(e => (e,trustForm(e.id,me)/* TODO: fill expires */))))
      }
    )
  }

  def trustAdd(i : Int, child : Int) = checkAdmin(i) { (request, entity) =>
    trustForm(child, entity.id).bindFromRequest()(request).fold(
      form => BadRequest(viewAdmin(request.account, entity)(trustResults = Seq((models.Entity.get(child), form)))),
      trust => {
        trust.add
        Redirect(routes.Entity.admin(entity.id))
      }
    )
  }

}
