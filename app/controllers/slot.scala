package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
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
    Ok(views.html.slot(slot))
  }

  type SlotForm = Form[String]
  private[this] val editForm = Form(single(
    "ident" -> nonEmptyText(1, 16)
  ))
  private[this] def editFormFill(s : Slot) = editForm.fill(s.ident)

  private[this] def viewEdit(slot : Slot)(
    editForm : SlotForm = editFormFill(slot))(
    implicit request : SiteRequest[_]) = {
    views.html.slotEdit(slot, editForm)
  }

  def edit(i : models.Slot.Id) = check(i, Permission.EDIT) { slot => implicit request =>
    Ok(viewEdit(slot)())
  }

  def change(i : models.Slot.Id) = check(i, Permission.EDIT) { slot => implicit request =>
    editFormFill(slot).bindFromRequest.fold(
      form => BadRequest(viewEdit(slot)(editForm = form)),
      { case ident =>
        slot.change(ident = ident)
        Redirect(slot.pageURL)
      }
    )
  }

  def create(s : models.Study.Id) = Study.check(s, Permission.CONTRIBUTE) { study => implicit request =>
    val form = editForm.bindFromRequest
    models.Slot.create(study, form.value).fold(Conflict : Result)(slot =>
      Created(viewEdit(slot)(editForm = form.fill(slot.ident))))
  }
}
