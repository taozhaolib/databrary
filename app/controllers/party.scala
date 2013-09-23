package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import org.mindrot.jbcrypt.BCrypt
import dbrary._
import util._
import models._

object Party extends SiteController {

  def view(i : models.Party.Id) = SiteAction { implicit request =>
    models.Party.get(i).fold(NotFound : Result)(
      e => Ok(views.html.party.view(e)))
  }

  def ajaxView() = SiteAction { implicit request =>
    request.user.fold(NotFound : Result)(
      e => Ok(views.html.modal.profile(request.identity)))
  }

  private def adminAccount(e : models.Party)(implicit request : UserRequest[_]) =
    cast[models.Account](e).filter(_.equals(request.identity))

  type EditForm = Form[(String, Option[Orcid], Option[(String, Option[String], String, String)])]
  private[this] def formFill(e : models.Party)(implicit request : UserRequest[_]) : EditForm = {
    val acct = adminAccount(e)
    Form(tuple(
      "name" -> nonEmptyText,
      "orcid" -> text(0,20).transform[Option[Orcid]](maybe(_).map(Orcid.apply _), _.fold("")(_.toString))
        .verifying(Messages("orcid.invalid"), _.fold(true)(_.valid)),
      "" -> MaybeMapping(acct.map(_ => tuple(
        "email" -> email,
        "password" -> optional(text(7)),
        "again" -> text,
        "openid" -> text(0,256)
      ).verifying(Messages("password.again"), f => f._2.fold(true)(_ == f._3))))
    )).fill((e.name, e.orcid, acct.map(a => (a.email, None, "", a.openid.getOrElse("")))))
  }

  def formForAccount(form : EditForm, party : Party)(implicit request : UserRequest[_]) =
    form.value.fold(adminAccount(party) : Option[Any])(_._3).isDefined

  private[this] def authorizeForm(child : models.Party.Id, parent : models.Party.Id, which : Boolean = false) : Form[Authorize] = Form(
    mapping(
      "access" -> Field.enum(Permission),
      "delegate" -> Field.enum(Permission),
      "pending" -> boolean,
      "expires" -> optional(sqlDate)
    )((access, delegate, pending, expires) => Authorize(
      child, parent, access, delegate, 
      if (pending || which) None else Some(new java.sql.Timestamp(System.currentTimeMillis)),
      expires.map(e => new java.sql.Timestamp(e.getTime))
    ))(t => 
      if (t.childId == child && t.parentId == parent)
        Some((t.access, t.delegate, t.authorized.fold(true)(_ => false), t.expires.map(e => new java.sql.Date(e.getTime))))
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

  private[this] def viewEdit(party : models.Party)(editForm : Option[EditForm] = None)(implicit request : UserRequest[_]) = {
    views.html.party.edit(party, editForm.getOrElse(formFill(party)))
  }

  private[this] def viewAdmin(party : models.Party)(
    authorizeChangeForm : Option[(models.Party,Form[Authorize])] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(models.Party,Form[Authorize])] = Seq())(implicit request : UserRequest[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    val authorizeForms = party.authorizeChildren(true).filter(t => Some(t.childId) != authorizeChange).map(t => (t.child, authorizeForm(t.childId, t.parentId).fill(t))) ++ authorizeChangeForm
    views.html.party.admin(party, authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults)
  }
  
  private[this] def checkAdmin(i : models.Party.Id, delegate : Boolean = true)(act : models.Party => UserRequest[AnyContent] => Result) = UserAction { implicit request =>
    if (request.identity.id != i && (!delegate || request.identity.delegatedBy(i) < Permission.ADMIN))
      Forbidden
    else
      act(models.Party.get(i).get)(request)
  }

  def edit(i : models.Party.Id) = checkAdmin(i) { party => implicit request =>
    Ok(viewEdit(party)())
  }

  def change(i : models.Party.Id) = checkAdmin(i) { party => implicit request =>
    formFill(party).bindFromRequest.fold(
      form => BadRequest(viewEdit(party)(editForm = Some(form))),
      { case (name, orcid, acct) =>
        party.change(name = name, orcid = orcid)
        acct foreach {
          case (email, password, _, openid) =>
            val acct = party.asInstanceOf[models.Account]
            acct.changeAccount(
              email = email,
              password = password.fold(acct.password)(BCrypt.hashpw(_, BCrypt.gensalt)),
              openid = maybe(openid))
        }
        Redirect(party.pageURL)
      }
    )
  }

  def admin(i : models.Party.Id) = checkAdmin(i) { party => implicit request =>
    Ok(viewAdmin(party)())
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
