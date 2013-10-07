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

object Container extends SiteController {
  type Request[A] = RequestObject[Volume]#Site[A]

  private[controllers] def action(v : models.Volume.Id, i : models.Container.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(v, models.Container.get(i)(_), p)

  private[controllers] def Action(v : models.Volume.Id, i : models.Container.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(v, i, p)

  def view(v : models.Volume.Id, i : models.Container.Id) = Action(v, i) { implicit request =>
    Ok(views.html.container.view(request.obj))
  }

  type ContainerForm = Form[(Option[String], Option[Date])]
  private[this] val editForm = Form(tuple(
    "name" -> optional(nonEmptyText),
    "date" -> optional(sqlDate)
  ))
  private[this] def editFormFill(c : Container) = editForm.fill((c.name, c.date))

  private[this] def viewEdit(container : Container)(
    editForm : ContainerForm = editFormFill(container))(
    implicit request : SiteRequest[_]) = {
    views.html.container.edit(Right(container), editForm)
  }

  def edit(v : models.Volume.Id, i : models.Container.Id) = Action(v, i, Permission.EDIT) { implicit request =>
    Ok(viewEdit(request.obj)())
  }

  def change(v : models.Volume.Id, i : models.Container.Id) = Action(v, i, Permission.EDIT) { implicit request =>
    editFormFill(request.obj).bindFromRequest.fold(
      form => BadRequest(viewEdit(request.obj)(editForm = form)),
      { case (name, date) =>
        request.obj.change(name = name, date = date)
        Redirect(request.obj.pageURL)
      }
    )
  }

  def create(s : models.Volume.Id) = Volume.Action(s, Permission.CONTRIBUTE) { implicit request =>
    Ok(views.html.container.edit(Left(request.obj), editForm))
  }

  def add(s : models.Volume.Id) = Volume.Action(s, Permission.CONTRIBUTE) { implicit request =>
    editForm.bindFromRequest.fold(
      form => BadRequest(views.html.container.edit(Left(request.obj), form)),
      { case (name, date) =>
        val cont = models.Container.create(request.obj, name = name, date = date)
        Redirect(cont.fullSlot.pageURL)
      }
    )
  }

}
