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

  private[controllers] def check(i : models.Slot.Id, p : Permission.Value = Permission.VIEW)(act : Slot => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Slot.get(i).fold(NotFound : Result) { s =>
      if (s.permission < p)
        Forbidden
      else
        act(s)(request)
    }
  }

  def view(v : models.Volume.Id, i : models.Slot.Id) = check(i) { slot => implicit request =>
    Ok(views.html.slot.view(slot))
  }

  type EditForm = Form[(Consent.Value, Boolean)]
  private[this] val editForm = Form(tuple(
    "consent" -> Field.enum(Consent),
    "toplevel" -> boolean
  ))
  private[this] def editFormFill(s : Slot) = editForm.fill((s.consent, s.toplevel))

  private[controllers] def viewEdit(slot : Slot)(
    editForm : EditForm = editFormFill(slot),
    recordForm : Record.SelectForm = Record.selectForm)(
    implicit request : SiteRequest[_]) = {
    views.html.slot.edit(slot, editForm, recordForm)
  }

  def edit(v : models.Volume.Id, i : models.Slot.Id) = check(i, Permission.EDIT) { slot => implicit request =>
    Ok(viewEdit(slot)())
  }

  def change(v : models.Volume.Id, i : models.Slot.Id) = check(i, Permission.EDIT) { slot => implicit request =>
    editFormFill(slot).bindFromRequest.fold(
      form => BadRequest(viewEdit(slot)(editForm = form)),
      { case (consent, toplevel) =>
        slot.change(consent = consent, toplevel = toplevel)
        Redirect(slot.pageURL)
      }
    )
  }

  type CreateForm = Form[(Option[Offset], Option[Offset])]
  private[this] val createForm : CreateForm = Form(tuple(
    "start" -> optional(Field.offset),
    "end" -> optional(Field.offset)
  ).verifying(Messages("range.invalid"), !_.zipped.exists(_ > _)))

  def create(v : models.Volume.Id, c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { cont => implicit request =>
    Ok(views.html.slot.create(cont, createForm))
  }

  def add(v : models.Volume.Id, c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { cont => implicit request =>
    createForm.bindFromRequest.fold(
      form => BadRequest(views.html.slot.create(cont, form)),
      { case (start, end) =>
        val slot = models.Slot.getOrCreate(cont, Range[Offset](start, end)(dbrary.PGSegment))
        Redirect(slot.pageURL)
      }
    )
  }

}
