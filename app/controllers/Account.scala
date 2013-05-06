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

object Account extends Controller {

  def admin = AccountAction { request => Ok(viewAdmin(request.account)()) }

  val accountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))

  def accountFormFill(a : Account) = accountForm.fill((a.email, a.openid.getOrElse("")))

  def trustForm(child : Int, parent : Int) : Form[Trust] = Form(mapping(
      "access" -> number(min=0, max=SitePermission.maxId-1),
      "delegate" -> number(min=0, max=UserPermission.maxId-1),
      "expires" -> default(sqlDate, new java.sql.Date(System.currentTimeMillis))
    )((access, delegate, expires) => Trust(child, parent, SitePermission(access), UserPermission(delegate), Some(new java.sql.Timestamp(expires.getTime)))
    )(t => t.expires.map(e => (t.access.id, t.delegate.id, new java.sql.Date(e.getTime))))
  )

  val trustSearchForm = Form(
    "name" -> nonEmptyText
  )

  def viewAdmin(account : Account)(
    accountForm : Form[(String, String)] = accountFormFill(account),
    trustChangeForm : Option[(Entity,Form[Trust])] = None,
    trustSearchForm : Form[String] = trustSearchForm,
    trustResults : Seq[(Entity,Form[Trust])] = Seq()) = {
    val trustForms = account.entity.trustChildren.map(t => (t.childEntity, trustForm(t.child, t.parent).fill(t)))
    views.html.accountAdmin(account, accountForm, trustForms, trustSearchForm, trustResults)
  }
  
  def accountChange = AccountAction { implicit request =>
    accountForm.bindFromRequest.fold(
      form => BadRequest(viewAdmin(request.account)(accountForm = form)),
      { case (email, openid) =>
        val a = request.account
        a.email = email
        a.openid = if (openid.isEmpty) None else Some(openid)
        a.commit
        Redirect(routes.Account.admin)
      }
    )
  }

  def trustChange(child : Int) = AccountAction { implicit request =>
    trustForm(child, request.account.id).bindFromRequest.fold(
      form => BadRequest(viewAdmin(request.account)(trustChangeForm = Some((models.Entity.get(child), form)))),
      trust => {
        trust.commit
        Redirect(routes.Account.admin)
      }
    )
  }

  def trustDelete(child : Int) = AccountAction { implicit request =>
    Trust.delete(child, request.account.id)
    Redirect(routes.Account.admin)
  }

  def trustSearch = AccountAction { implicit request =>
    val form = trustSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(request.account)(trustSearchForm = form)),
      name => {
        val me = request.account.id
        val res = DB.withSession { implicit session =>
          models.Entity.byName(name).filter(e => e.id =!= me && e.id.notIn(Trust.byParent(me).map(_.child))).take(8).list
        }
        Ok(viewAdmin(request.account)(trustSearchForm = form, 
          trustResults = res.map(e => (e,trustForm(e.id,me)))))
      }
    )
  }

  def trustAdd(child : Int) = AccountAction { implicit request =>
    trustForm(child, request.account.id).bindFromRequest.fold(
      form => BadRequest(viewAdmin(request.account)(trustResults = Seq((models.Entity.get(child), form)))),
      trust => {
        trust.add
        Redirect(routes.Account.admin)
      }
    )
  }

}
