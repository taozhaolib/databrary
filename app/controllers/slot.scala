package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import java.sql.Date
import dbrary.{Offset,Range}
import models._

object Slot extends SiteController {
  type Request[A] = RequestObject[Slot]#Site[A]

  private[controllers] def action(v : models.Volume.Id, i : models.Slot.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(v, models.Slot.get(i)(_), p)

  private[controllers] def Action(v : models.Volume.Id, i : models.Slot.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(v, i, p)

  def view(v : models.Volume.Id, i : models.Slot.Id) = Action(v, i) { implicit request =>
    Ok(views.html.slot.view(request.obj))
  }

  type EditForm = Form[(Option[(Option[String], Option[Date])], Consent.Value)]
  private[this] def editForm(container : Boolean) = Form(tuple(
    "" -> MaybeMapping(if (container) Some(tuple(
      "name" -> optional(nonEmptyText),
      "date" -> optional(sqlDate)
    )) else None),
    "consent" -> Field.enum(Consent)
  ))
  private[this] def editFormFill(s : Slot) = {
    val full = s.isFull
    val cont = (if (full) Some(s.container) else None)
    editForm(full).fill((cont.map(c => (c.name, c.date)), s.consent))
  }

  def formForContainer(form : EditForm, slot : Slot) =
    form.value.fold(slot.isFull)(_._1.isDefined)

  private[controllers] def viewEdit(slot : Slot)(
    editForm : EditForm = editFormFill(slot),
    recordForm : Record.SelectForm = Record.selectForm)(
    implicit request : SiteRequest[_]) = {
    views.html.slot.edit(slot, editForm, recordForm)
  }

  def edit(v : models.Volume.Id, i : models.Slot.Id) = Action(v, i, Permission.EDIT) { implicit request =>
    Ok(viewEdit(request.obj)())
  }

  def change(v : models.Volume.Id, i : models.Slot.Id) = Action(v, i, Permission.EDIT) { implicit request =>
    editFormFill(request.obj).bindFromRequest.fold(
      form => BadRequest(viewEdit(request.obj)(editForm = form)),
      { case (container, consent) =>
        container foreach {
          case (name, date) => request.obj.container.change(name = name, date = date)
        }
        request.obj.change(consent = consent)
        Redirect(request.obj.pageURL)
      }
    )
  }

  type CreateForm = Form[(Option[Offset], Option[Offset])]
  private[this] val createForm : CreateForm = Form(tuple(
    "start" -> optional(of[Offset]),
    "end" -> optional(of[Offset])
  ).verifying(Messages("range.invalid"), !_.zipped.exists(_ > _)))

  def create(v : models.Volume.Id, c : models.Container.Id) = Container.Action(v, c, Permission.CONTRIBUTE) { implicit request =>
    Ok(views.html.slot.create(request.obj, createForm))
  }

  def add(v : models.Volume.Id, c : models.Container.Id) = Container.Action(v, c, Permission.CONTRIBUTE) { implicit request =>
    createForm.bindFromRequest.fold(
      form => BadRequest(views.html.slot.create(request.obj, form)),
      { case (start, end) =>
        val slot = models.Slot.getOrCreate(request.obj, Range[Offset](start, end)(dbrary.PGSegment))
        Redirect(slot.pageURL)
      }
    )
  }

  type CommentForm = Form[String]
  val commentForm : CommentForm = Form("text" -> nonEmptyText)

  def comment(v : models.Volume.Id, s : models.Slot.Id) = (SiteAction.access(Permission.VIEW) ~> action(v, s)) { implicit request =>
    commentForm.bindFromRequest().fold(
      form => BadRequest(views.html.slot.view(request.obj, form)),
      { text =>
        request.obj.postComment(text)(request.asInstanceOf[AuthSite])
        Redirect(request.obj.pageURL)
      }
    )
  }

  def tag(v : models.Volume.Id, s : models.Slot.Id, t : String, up : Option[Boolean]) = (SiteAction.access(Permission.VIEW) ~> action(v, s)) { implicit request =>
    if (!request.obj.setTag(t, up)(request.asInstanceOf[AuthSite]))
      BadRequest(views.html.slot.view(request.obj))
    else
      Redirect(request.obj.pageURL)
  }
}
