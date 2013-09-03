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

  type SlotForm = Form[(Consent.Value, Date)]
  private[this] val editForm = Form(tuple(
    "consent" -> Field.enum(Consent),
    "date" -> sqlDate
  ))
  private[this] def editFormFill(s : Slot) = editForm.fill((s.consent, s.date))

  private[this] def viewEdit(slot : Slot)(
    editForm : SlotForm = editFormFill(slot))(
    implicit request : SiteRequest[_]) = {
    views.html.slot.edit(Right(slot), editForm)
  }

  def edit(i : models.Slot.Id) = check(i, Permission.EDIT) { slot => implicit request =>
    Ok(viewEdit(slot)())
  }

  def change(i : models.Slot.Id) = check(i, Permission.EDIT) { slot => implicit request =>
    val form = editFormFill(slot).bindFromRequest
    form.fold(
      form => BadRequest(viewEdit(slot)(editForm = form)),
      { case (consent, date) =>
        slot.change(consent = consent, date = date)
        Redirect(slot.pageURL)
      }
    )
  }

  def create(s : models.Study.Id) = Study.check(s, Permission.CONTRIBUTE) { study => implicit request =>
    Ok(views.html.slot.edit(Left(study), editForm))
  }

  def add(s : models.Study.Id) = Study.check(s, Permission.CONTRIBUTE) { study => implicit request =>
    val form = editForm.bindFromRequest
    form.fold(
      form => BadRequest(views.html.slot.edit(Left(study), form)),
      { case (consent, date) =>
        val slot = models.Slot.create(study = study, consent = consent, date = date)
        Created(views.html.slot.view(slot))
      }
    )
  }
}
