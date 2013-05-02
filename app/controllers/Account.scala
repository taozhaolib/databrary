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

  def home = AccountAction { viewHome(_) }

  val accountForm = Form(tuple(
    "email" -> email,
    "openid" -> text
  ))

  def accountFormFill(a : Account) = accountForm.fill((a.email, a.openid.getOrElse("")))

  def authorizeForm(parent : Entity) = Form(mapping(
      "name" -> optional(text),
      "child" -> number,
      "parent" -> ignored(parent.id),
      "access" -> number(min=0, max=SitePermission.maxId-1),
      "delegate" -> number(min=0, max=UserPermission.maxId-1),
      "expires" -> sqlDate
    )((name, child, parent, access, delegate, expires) => Trust(child, parent, SitePermission(access), UserPermission(delegate), Some(new java.sql.Timestamp(expires.getTime)))
    )(t => t.expires.map(e => (Some(t.childEntity.name), t.child, t.parent, t.access.id, t.delegate.id, new java.sql.Date(e.getTime))))
  )

  def viewHome(implicit request : AccountRequest[_]) = {
    val acct = request.account
    val form = authorizeForm(acct.entity)
    Ok(views.html.home(acct, accountFormFill(acct), Trust.getChildren(acct.id).map(form.fill(_))))
  }
  
  def postAccount = AccountAction { implicit request =>
    accountForm.bindFromRequest.fold(
      form => BadRequest(views.html.home(request.account, form, Seq())),
      { case (email, openid) =>
        val a = request.account
        a.email = email
        a.openid = if (openid.isEmpty) None else Some(openid)
        a.commit
        Redirect(routes.Account.home)
      }
    )
  }

  def postAuthorize = AccountAction { implicit request =>
    authorizeForm(request.account.entity).bindFromRequest.fold(
      form => BadRequest(views.html.home(request.account, accountFormFill(request.account), Seq(form))),
      trust => {
        trust.commit
        Redirect(routes.Account.home)
      }
    )
  }
}
