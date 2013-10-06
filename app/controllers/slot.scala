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

  /* FIXME: access is wrong */
  private[controllers] def check(v : models.Volume.Id, i : models.Slot.Id, p : Permission.Value = Permission.VIEW, a : Permission.Value = Permission.NONE)(act : Slot => SiteRequest[AnyContent] => Result) = SiteAction.access(a) { implicit request =>
    models.Slot.get(i).filter(_.volumeId == v).fold(NotFound : Result) { s =>
      if (s.permission < p)
        Forbidden
      else
        act(s)(request)
    }
  }

  def view(v : models.Volume.Id, i : models.Slot.Id) = check(v, i) { slot => implicit request =>
    Ok(views.html.slot.view(slot))
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

  def edit(v : models.Volume.Id, i : models.Slot.Id) = check(v, i, Permission.EDIT) { slot => implicit request =>
    Ok(viewEdit(slot)())
  }

  def change(v : models.Volume.Id, i : models.Slot.Id) = check(v, i, Permission.EDIT) { slot => implicit request =>
    editFormFill(slot).bindFromRequest.fold(
      form => BadRequest(viewEdit(slot)(editForm = form)),
      { case (container, consent) =>
        container foreach {
          case (name, date) => slot.container.change(name = name, date = date)
        }
        slot.change(consent = consent)
        Redirect(slot.pageURL)
      }
    )
  }

  type CreateForm = Form[(Option[Offset], Option[Offset])]
  private[this] val createForm : CreateForm = Form(tuple(
    "start" -> optional(of[Offset]),
    "end" -> optional(of[Offset])
  ).verifying(Messages("range.invalid"), !_.zipped.exists(_ > _)))

  def create(v : models.Volume.Id, c : models.Container.Id) = Container.check(v, c, Permission.CONTRIBUTE) { cont => implicit request =>
    Ok(views.html.slot.create(cont, createForm))
  }

  def add(v : models.Volume.Id, c : models.Container.Id) = Container.check(v, c, Permission.CONTRIBUTE) { cont => implicit request =>
    createForm.bindFromRequest.fold(
      form => BadRequest(views.html.slot.create(cont, form)),
      { case (start, end) =>
        val slot = models.Slot.getOrCreate(cont, Range[Offset](start, end)(dbrary.PGSegment))
        Redirect(slot.pageURL)
      }
    )
  }

  type CommentForm = Form[String]
  val commentForm : CommentForm = Form("text" -> nonEmptyText)

  def comment(v : models.Volume.Id, s : models.Slot.Id) = Slot.check(v, s, Permission.VIEW, Permission.VIEW) { slot => implicit request =>
    commentForm.bindFromRequest().fold(
      form => BadRequest(views.html.slot.view(slot, form)),
      { text =>
        slot.postComment(text)(request.asInstanceOf[AuthSite])
        Redirect(slot.pageURL)
      }
    )
  }

  def tag(v : models.Volume.Id, s : models.Slot.Id, t : String, up : Option[Boolean]) = Slot.check(v, s, Permission.VIEW, Permission.VIEW) { slot => implicit request =>
    if (!slot.setTag(t, up)(request.asInstanceOf[AuthSite]))
      BadRequest(views.html.slot.view(slot))
    else
      Redirect(slot.pageURL)
  }
}
