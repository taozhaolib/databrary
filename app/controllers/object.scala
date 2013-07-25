package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import util._
import models._

object Object extends SiteController {

  private[controllers] def check(i : models.Container.Id, o : models.Object.Id, p : Permission.Value = Permission.VIEW)(act : ObjectLink => SiteRequest[AnyContent] => Result) = Container.check(i) { container => implicit request =>
    container.getObject(o).fold(NotFound : Result) { link =>
      if (link.permission < p)
        Forbidden
      else
        act(link)(request)
    }
  }

  def view(i : models.Container.Id, o : models.Object.Id) = check(i, o) { link => implicit request =>
    Ok(views.html.objectLink(link))
  }

  private[this] val linkFields = tuple(
    "title" -> nonEmptyText,
    "description" -> text
  )
  private[this] val fileFields = tuple(
    "consent" -> form.enumField(Consent),
    "date" -> optional(sqlDate)
  )

  type EditForm = Form[((String, String), Option[(Consent.Value, Option[java.sql.Date])])]
  private[this] def formFill(link : ObjectLink)(implicit site : Site) : EditForm = {
    /* Only allow file parameters to be changed if this is the original study for this object */
    val file = Option(link.obj.asInstanceOf[models.FileObject]).filter(_.ownerId == Some(link.container.studyId))
    Form(tuple(
      "" -> linkFields,
      "" -> MaybeMapping(file.map(_ => fileFields))
    )).fill(((link.title, link.description.getOrElse("")), file.map(f => (f.consent, f.date))))
  }

  def edit(s : models.Container.Id, o : models.Object.Id) = check(s, o, Permission.EDIT) { link => implicit request =>
    Ok(views.html.objectEdit(link, formFill(link)))
  }

  def change(s : models.Container.Id, o : models.Object.Id) = check(s, o, Permission.EDIT) { link => implicit request =>
    formFill(link).bindFromRequest.fold(
      form => BadRequest(views.html.objectEdit(link, form)), {
      case ((title, description), file) =>
        link.change(title = title, description = maybe(description))
        file foreach {
          case (consent, date) => link.obj.asInstanceOf[models.FileObject].change(consent = consent, date = date)
        }
        Redirect(link.pageURL)
      }
    )
  }

  type UploadForm = Form[(String, String, (Consent.Value, Option[java.sql.Date]), Unit)]
  private[this] val uploadForm = Form(tuple(
    "title" -> text,
    "description" -> text,
    "" -> fileFields,
    "file" -> ignored(())
  ))

  def create(c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { container => implicit request =>
    Ok(views.html.objectCreate(container, uploadForm))
  }

  def upload(c : models.Container.Id) = Container.check(c, Permission.CONTRIBUTE) { container => implicit request =>
    val form = uploadForm.bindFromRequest
    val file = request.body.asMultipartFormData.flatMap(_.file("file"))
    (if (file.isEmpty) form.withError("file", "error.required") else form).fold(
      form => BadRequest(views.html.objectCreate(container, form)), {
      case (title, description, (consent, date), ()) =>
        val f = file.get
        f.contentType.flatMap(ObjectFormat.getMimetype(_)).fold(
          BadRequest(views.html.objectCreate(container, form.withError("file", "file.format.unknown", f.contentType.getOrElse("unknown")))) : Result)
        { format =>
          val obj =
            if (format.timeseries)
              ???
            else
              FileObject.create(format, container.studyId, consent, date)
          val link = ObjectLink.create(container, obj, maybe(title).getOrElse(f.filename), maybe(description))
          Redirect(link.pageURL)
        }
      }
    )
  }

}
