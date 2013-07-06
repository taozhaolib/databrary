package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Study extends SiteController {

  private[this] def check(i : models.Study.Id, p : Permission.Value = Permission.VIEW)(act : Study => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Study.get(i).fold(NotFound : Result) { s =>
      if (s.permission < p)
        Forbidden
      else
        act(s)(request)
    }
  }

  def view(i : models.Study.Id) = check(i) { study => implicit request =>
    Ok(views.html.study(study))
  }

  private[this] val editForm = Form(tuple(
    "title" -> nonEmptyText,
    "description" -> optional(text)
  ))
  private[this] def editFormFill(s : Study) = editForm.fill((s.title, s.description))

  private[this] def accessForm(study : Study, entity : Identity.Id) : Form[StudyAccess] = Form(
    mapping(
      "access" -> number(min=0, max=Permission.maxId-1),
      "inherit" -> number(min=0, max=(if (entity.unId > 0) Permission.EDIT else Permission.DOWNLOAD).id)
    )((access, inherit) => StudyAccess(
      study.id, entity, 
      Permission(access.max(inherit)),
      Permission(inherit)
    ))(a =>
      if (a.studyId == study.id && a.entityId == entity)
        Some((a.access.id, a.inherit.id))
      else
        None
    )
  )

  private[this] val accessSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewEdit(study : Study)(
    editForm : Form[(String,Option[String])] = editFormFill(study),
    accessChangeForm : Option[(Identity,Form[StudyAccess])] = None,
    accessSearchForm : Form[String] = accessSearchForm,
    accessResults : Seq[(Identity,Form[StudyAccess])] = Seq())(
    implicit request : SiteRequest[_]) = {
    val accessChange = accessChangeForm.map(_._1.id)
    val accessForms = study.entityAccess().filter(a => Some(a.entityId) != accessChange).map(a => (a.entity, accessForm(study, a.entityId).fill(a))) ++ accessChangeForm
    views.html.studyEdit(study, editForm, accessForms, accessSearchForm, accessResults)
  }

  def edit(i : models.Study.Id) = check(i, Permission.EDIT) { study => implicit request =>
    Ok(viewEdit(study)())
  }

  def change(i : models.Study.Id) = check(i, Permission.EDIT) { study => implicit request =>
    editFormFill(study).bindFromRequest.fold(
      form => BadRequest(viewEdit(study)(editForm = form)),
      { case (title, description) =>
        study.change(title = title, description = description.flatMap(maybe(_)))
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def accessChange(i : models.Study.Id, e : Identity.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    accessForm(study, e).bindFromRequest.fold(
      form => BadRequest(viewEdit(study)(accessChangeForm = Some((Identity.get(e), form)))),
      access => {
        access.set
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def accessDelete(i : models.Study.Id, e : Identity.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    if (e != request.identity.id)
      StudyAccess.delete(study.id, e)
    Redirect(routes.Study.edit(study.id))
  }

  def accessSearch(i : models.Study.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    val form = accessSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewEdit(study)(accessSearchForm = form)),
      name => {
        val res = Identity.searchForStudyAccess(name, study.id)
        Ok(viewEdit(study)(accessSearchForm = form, 
          accessResults = res.map(e => (e,accessForm(study,e.id)))))
      }
    )
  }

  def accessAdd(i : models.Study.Id, e : Identity.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    accessForm(study, e).bindFromRequest.fold(
      form => BadRequest(viewEdit(study)(accessResults = Seq((Identity.get(e),form)))),
      access => {
        access.set
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def create(e : Option[Identity.Id]) = UserAction { implicit request =>
    val owner = e.getOrElse(request.identity.id)
    if (request.identity.delegatedBy(owner) < Permission.CONTRIBUTE)
      Forbidden
    else {
      val form = editForm.bindFromRequest
      val study = (models.Study.create _).tupled(form.value.getOrElse(("New study", None)))
      StudyAccess(study.id, owner, Permission.ADMIN, Permission.CONTRIBUTE).set
      Ok(viewEdit(study)(editForm = form))
    }
  }

  private[this] def checkObject(i : models.Study.Id, o : models.Object.Id, p : Permission.Value = Permission.VIEW)(act : StudyObject => SiteRequest[AnyContent] => Result) = check(i) { study => implicit request =>
    study.getObject(o).fold(NotFound : Result) { obj =>
      if (obj.permission < p)
        Forbidden
      else
        act(obj)(request)
    }
  }

  def viewObject(i : models.Study.Id, o : models.Object.Id) = checkObject(i, o) { obj => implicit request =>
    Ok(views.html.studyObject(obj))
  }
}
