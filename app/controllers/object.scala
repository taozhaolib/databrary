package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Object extends SiteController {
  private[controllers] def check(i : models.Study.Id, o : models.Object.Id, p : Permission.Value = Permission.VIEW)(act : StudyObject => SiteRequest[AnyContent] => Result) = Study.check(i) { study => implicit request =>
    study.getObject(o).fold(NotFound : Result) { obj =>
      if (obj.permission < p)
        Forbidden
      else
        act(obj)(request)
    }
  }

  def view(i : models.Study.Id, o : models.Object.Id) = check(i, o) { obj => implicit request =>
    Ok(views.html.studyObject(obj))
  }

  type EditForm = Form[(String, String, Consent.Value, Option[java.sql.Date])]
  private[this] val editForm = Form(tuple(
    "title" -> nonEmptyText,
    "description" -> text,
    "consent" -> form.enumField(Consent),
    "date" -> optional(sqlDate)
  ))
  private[this] def editFormFill(o : StudyObject) = editForm.fill((o.title, o.description.getOrElse(""), o.obj.consent, o.obj.date))

  def edit(s : models.Study.Id, o : models.Object.Id) = check(s, o, Permission.EDIT) { obj => implicit request =>
    Ok(views.html.objectEdit(obj, editFormFill(obj)))
  }

  def change(s : models.Study.Id, o : models.Object.Id) = check(s, o, Permission.EDIT) { obj => implicit request =>
    editFormFill(obj).bindFromRequest.fold(
      form => BadRequest(views.html.objectEdit(obj, editForm = form)),
      { case (title, description, consent, date) =>
        obj.change(title = title, description = maybe(description))
        obj.obj.change(consent = consent, date = date)
        Redirect(routes.Object.view(obj.studyId, obj.objId))
      }
    )
  }
}
