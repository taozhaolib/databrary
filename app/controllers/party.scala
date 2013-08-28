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

object Party extends SiteController {

  def view(i : models.Party.Id) = SiteAction { implicit request =>
    models.Party.get(i).fold(NotFound : Result)(
      e => Ok(views.html.party(e, e.delegated)))
  }

  type EditForm = Form[(String, Option[Orcid], Option[(String, String)])]
  private[this] def formFill(e : models.Party)(implicit request : UserRequest[_]) : EditForm = {
    val acct = cast[models.Account](e).filter(_.equals(request.account))
    Form(tuple(
      "name" -> nonEmptyText,
      "orcid" -> text(0,20).transform[Option[Orcid]](maybe(_).map(Orcid.apply _), _.fold("")(_.toString))
        .verifying("invalid ORCID iD", _.fold(true)(_.valid)),
      "" -> MaybeMapping(acct.map(_ => tuple(
        "email" -> email,
        "openid" -> text(0,256)
      )))
    )).fill((e.name, e.orcid, acct.map(a => (a.email, a.openid.getOrElse("")))))
  }

  def formForAccount(form : EditForm) = form.value.fold(false)(!_._3.isEmpty)

  private[this] def authorizeForm(child : models.Party.Id, parent : models.Party.Id, which : Boolean = false) : Form[Authorize] = Form(
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

  private[this] def authorizeFormWhich(me : models.Party, other : models.Party.Id, which : Boolean) =
    if (which)
      authorizeForm(me.id, other, which)
    else
      authorizeForm(other, me.id, which)

  private[this] val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewAdmin(party : models.Party)(
    editForm : Option[EditForm] = None,
    authorizeChangeForm : Option[(models.Party,Form[Authorize])] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(models.Party,Form[Authorize])] = Seq())(
    implicit request : UserRequest[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    val authorizeForms = party.authorizeChildren(true).filter(t => Some(t.childId) != authorizeChange).map(t => (t.child, authorizeForm(t.childId, t.parentId).fill(t))) ++ authorizeChangeForm
    views.html.partyAdmin(party, editForm.getOrElse(formFill(party)), authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults)
  }
  
  private[this] def checkAdmin(i : models.Party.Id, delegate : Boolean = true)(act : models.Party => UserRequest[AnyContent] => Result) = UserAction { implicit request =>
    if (request.identity.id != i && (!delegate || request.identity.delegatedBy(i) < Permission.ADMIN))
      Forbidden
    else
      act(models.Party.get(i).get)(request)
  }

  def admin(i : models.Party.Id) = checkAdmin(i) { party => implicit requset =>
    Ok(viewAdmin(party)()) 
  }

  def change(i : models.Party.Id) = checkAdmin(i) { party => implicit request =>
    formFill(party).bindFromRequest.fold(
      form => BadRequest(viewAdmin(party)(editForm = Some(form))),
      { case (name, orcid, acct) =>
        party.change(name = name, orcid = orcid)
        acct foreach {
          case (email, openid) =>
            party.asInstanceOf[models.Account].changeAccount(email = email, openid = maybe(openid))
        }
        Redirect(party.pageURL)
      }
    )
  }

  def authorizeChange(i : models.Party.Id, child : models.Party.Id) = checkAdmin(i) { party => implicit request =>
    authorizeForm(child, party.id).bindFromRequest.fold(
      form => BadRequest(viewAdmin(party)(authorizeChangeForm = Some((models.Party.get(child).get, form)))),
      authorize => {
        authorize.set
        Redirect(routes.Party.admin(party.id))
      }
    )
  }

  def authorizeDelete(i : models.Party.Id, child : models.Party.Id) = checkAdmin(i) { party => implicit request =>
    models.Authorize.delete(child, party.id)
    Redirect(routes.Party.admin(party.id))
  }

  def authorizeSearch(i : models.Party.Id, which : Boolean) = checkAdmin(i) { party => implicit request =>
    val form = authorizeSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(party)(authorizeWhich = Some(which), authorizeSearchForm = form)),
      name => {
        val res = models.Party.searchForAuthorize(name, party.id)
        Ok(viewAdmin(party)(authorizeWhich = Some(which), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e,authorizeFormWhich(party, e.id, which)
            /* TODO: fill expires */))))
      }
    )
  }

  def authorizeAdd(i : models.Party.Id, which : Boolean, other : models.Party.Id) = checkAdmin(i) { party => implicit request =>
    authorizeFormWhich(party, other, which).bindFromRequest.fold(
      form => BadRequest(viewAdmin(party)(authorizeWhich = Some(which), authorizeResults = Seq((models.Party.get(other).get, form)))),
      authorize => {
        authorize.set
        Redirect(routes.Party.admin(party.id))
      }
    )
  }
}
