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

  private[controllers] def check(i : models.Container.Id, p : Permission.Value = Permission.VIEW)(act : Container => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Container.get(i).fold(NotFound : Result) { c =>
      if (c.permission < p)
        Forbidden
      else
        act(c)(request)
    }
  }

  def view(vid : models.Volume.Id, i : models.Container.Id) = check(i) { container => implicit request =>
    Ok(views.html.container.view(container))
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

  def edit(vid : models.Volume.Id, i : models.Container.Id) = check(i, Permission.EDIT) { cont => implicit request =>
    Ok(viewEdit(cont)())
  }

  def change(vid : models.Volume.Id, i : models.Container.Id) = check(i, Permission.EDIT) { cont => implicit request =>
    editFormFill(cont).bindFromRequest.fold(
      form => BadRequest(viewEdit(cont)(editForm = form)),
      { case (name, date) =>
        cont.change(name = name, date = date)
        Redirect(cont.pageURL)
      }
    )
  }

  def create(s : models.Volume.Id) = Volume.check(s, Permission.CONTRIBUTE) { volume => implicit request =>
    Ok(views.html.container.edit(Left(volume), editForm))
  }

  def add(s : models.Volume.Id) = Volume.check(s, Permission.CONTRIBUTE) { volume => implicit request =>
    editForm.bindFromRequest.fold(
      form => BadRequest(views.html.container.edit(Left(volume), form)),
      { case (name, date) =>
        val cont = models.Container.create(volume, name = name, date = date)
        Redirect(cont.pageURL)
      }
    )
  }

}
