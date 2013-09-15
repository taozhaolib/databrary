package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import java.sql.Date
import models._

object Slot extends SiteController {

  private[controllers] def check(i : models.Slot.Id, p : Permission.Value = Permission.VIEW)(act : Slot => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Slot.get(i).fold(NotFound : Result) { s =>
      if (s.permission < p)
        Forbidden
      else
        act(s)(request)
    }
  }

  def view(i : models.Slot.Id) = check(i) { slot => implicit request =>
    Ok(views.html.slot.view(slot))
  }

  type EditForm = Form[(Consent.Value)]
  private[this] val editForm = Form(
    "consent" -> Field.enum(Consent)
  )
  private[this] def editFormFill(s : Slot) = editForm.fill((s.consent))

  private[this] def viewEdit(slot : Slot)(
    editForm : SlotForm = editFormFill(slot))(
    implicit request : SiteRequest[_]) = {
    views.html.slot.edit(slot, editForm)
  }

  def edit(i : models.Slot.Id) = check(i, Permission.EDIT) { slot => implicit request =>
    Ok(viewEdit(slot)())
  }

  def change(i : models.Slot.Id) = check(i, Permission.EDIT) { slot => implicit request =>
    editFormFill(slot).bindFromRequest.fold(
      form => BadRequest(viewEdit(slot)(editForm = form)),
      { case (consent) =>
        slot.change(consent = consent)
        Redirect(slot.pageURL)
      }
    )
  }

  type CreateForm = Form[(Offset, Offset)]
  private[this] val createForm = Form(tuple(
    "start" -> optional(Field.offset),
    "end" -> optional(Field.offset)
  ).verifying(Messages("range.invalid"), {
    case (Some(s), Some(e)) => s <= e
    case _ => true
  }))

  def create(c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { cont => implicit request =>
    Ok(views.html.slot.create(cont, createForm))
  }

  def add(c : models.Container.Id) = Container.check(s, Permission.CONTRIBUTE) { cont => implicit request =>
    createForm.bindFromRequest.fold(
      form => BadRequest(views.html.slot.create(cont), form),
      { case (start, end) =>
        val slot = models.Slot.getOrCreate(cont, Range[Offset](start, end)(PGSegment))
        Redirect(slot.pageURL)
      }
    )
  }

}
