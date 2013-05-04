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

  def home = AccountAction { request => Ok(viewHome(request.account)()) }

  val accountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))

  def accountFormFill(a : Account) = accountForm.fill((a.email, a.openid.getOrElse("")))

  def trustForm(child : Int, parent : Int) : Form[Trust] = Form(mapping(
      "access" -> number(min=0, max=SitePermission.maxId-1),
      "delegate" -> number(min=0, max=UserPermission.maxId-1),
      "expires" -> sqlDate
    )((access, delegate, expires) => Trust(child, parent, SitePermission(access), UserPermission(delegate), Some(new java.sql.Timestamp(expires.getTime)))
    )(t => t.expires.map(e => (t.access.id, t.delegate.id, new java.sql.Date(e.getTime))))
  )

  val trustSearchForm = Form(
    "name" -> text
  )

  def viewHome(account : Account)(
    accountForm : Form[(String, String)] = accountFormFill(account),
    trustChangeForm : Option[(Entity,Form[Trust])] = None,
    trustSearchForm : Form[String] = trustSearchForm) = {
    val trustForms = account.entity.trustChildren.map(t => (t.childEntity, trustForm(t.child, t.parent).fill(t)))
    views.html.home(account, accountForm, trustForms, trustSearchForm)
  }
  
  def accountChange = AccountAction { implicit request =>
    accountForm.bindFromRequest.fold(
      form => BadRequest(viewHome(request.account)(accountForm = form)),
      { case (email, openid) =>
        val a = request.account
        a.email = email
        a.openid = if (openid.isEmpty) None else Some(openid)
        a.commit
        Redirect(routes.Account.home)
      }
    )
  }

  def trustChange(child : Int) = AccountAction { implicit request =>
    trustForm(child, request.account.id).bindFromRequest.fold(
      form => BadRequest(viewHome(request.account)(trustChangeForm = Some((Entity.get(child), form)))),
      trust => {
        trust.commit
        Redirect(routes.Account.home)
      }
    )
  }

  def trustDelete(child : Int) = AccountAction { implicit request =>
    Trust.delete(child, request.account.id)
    Redirect(routes.Account.home)
  }

  def trustSearch = AccountAction { implicit request =>
    trustSearchForm.bindFromRequest.fold(
      form => BadRequest(viewHome(request.account)(trustSearchForm = form)),
      trust => {
        Redirect(routes.Account.home)
      }
    )
  }
}
