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

  type LinkForm = Form[(String, String)]
  private[this] val linkForm = Form(tuple(
    "title" -> nonEmptyText,
    "description" -> text
  ))

  type FileForm = Form[(Consent.Value, Option[java.sql.Date])]
  private[this] val fileForm = Form(tuple(
    "consent" -> form.enumField(Consent),
    "date" -> optional(sqlDate)
  ))

  type Forms = (LinkForm, Option[FileForm])
  private[this] def formsFill(link : ObjectLink)(implicit site : Site) : Forms =
    ( linkForm.fill((link.title, link.description.getOrElse("")))
    , Option(link.obj.asInstanceOf[models.FileObject]) map (file =>
        fileForm.fill((file.consent, file.date)))
    )

  def edit(s : models.Container.Id, o : models.Object.Id) = check(s, o, Permission.EDIT) { link => implicit request =>
    val f = formsFill(link)
    Ok(views.html.objectEdit(link, f._1, f._2))
  }

  def change(s : models.Container.Id, o : models.Object.Id) = check(s, o, Permission.EDIT) { link => implicit request =>
    val forms = formsFill(link)
    val linkForm = forms._1.bindFromRequest
    val fileForm = forms._2.map(_.bindFromRequest)
    val v = for {
      l <- linkForm.value
      f <- fileForm.fold(Some(None) : Option[Option[(Consent.Value, Option[java.sql.Date])]])(_.value.map(Some(_)))
    } yield (l, f)
    v.fold(BadRequest(views.html.objectEdit(link, linkForm, fileForm)) : Result) {
      case ((title, description), file) =>
        link.change(title = title, description = maybe(description))
        file foreach {
          case (consent, date) => link.obj.asInstanceOf[models.FileObject].change(consent = consent, date = date)
        }
        Redirect(routes.Object.view(s, o))
    }
  }
}
