package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          db.slick.DB
import          db.slick.Config.driver.simple._
import          i18n.Messages
import dbrary.Permission
import models._

object Study extends SiteController {

  private[this] def check(i : Int, p : Permission.Value = Permission.VIEW)(act : Study => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Study.get(i).fold(NotFound : Result)(s =>
      if (s.permission < p)
        Forbidden
      else
        act(s)(request))
  }

  def view(i : Int) = check(i) { study => implicit request =>
    Ok(views.html.study(study))
  }

  private[this] val editForm = Form(tuple(
    "title" -> nonEmptyText,
    "description" -> text
  ))
  private[this] def editFormFill(s : Study) = editForm.fill((s.title, s.description.getOrElse("")))

  private[this] def accessForm(study : Study, entity : Int) : Form[StudyAccess] = Form(
    mapping(
      "access" -> number(min=0, max=Permission.maxId-1),
      "inherit" -> number(min=0, max=(if (entity > 0) Permission.EDIT else Permission.DOWNLOAD).id)
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
    editForm : Form[(String,String)] = editFormFill(study),
    accessChangeForm : Option[(Identity,Form[StudyAccess])] = None,
    accessSearchForm : Form[String] = accessSearchForm,
    accessResults : Seq[(Identity,Form[StudyAccess])] = Seq())(
    implicit request : SiteRequest[_]) = {
    val accessChange = accessChangeForm.map(_._1.id)
    val accessForms = study.entityAccess().filter(a => Some(a.entityId) != accessChange).map(a => (a.entity, accessForm(study, a.entityId).fill(a))) ++ accessChangeForm
    views.html.studyEdit(study, editForm, accessForms, accessSearchForm, accessResults)
  }

  def edit(i : Int) = check(i, Permission.EDIT) { study => implicit request =>
    Ok(viewEdit(study)())
  }

  def change(i : Int) = check(i, Permission.EDIT) { study => implicit request =>
    editFormFill(study).bindFromRequest.fold(
      form => BadRequest(viewEdit(study)(editForm = form)),
      { case (title, description) =>
        study.change(title = title, description = maybe(description))
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def accessChange(i : Int, e : Int) = check(i, Permission.ADMIN) { study => implicit request =>
    accessForm(study, e).bindFromRequest.fold(
      form => BadRequest(viewEdit(study)(accessChangeForm = Some((Identity.get(e), form)))),
      access => {
        access.set
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def accessDelete(i : Int, e : Int) = check(i, Permission.ADMIN) { study => implicit request =>
    StudyAccess.get(study.id, e).filter(_.entityId != request.identity.id).map(_.remove)
    Redirect(routes.Study.edit(study.id))
  }

  def accessSearch(i : Int) = check(i, Permission.ADMIN) { study => implicit request =>
    val form = accessSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewEdit(study)(accessSearchForm = form)),
      name => {
        val res = Identity.searchForStudyAccess(name, study)
        Ok(viewEdit(study)(accessSearchForm = form, 
          accessResults = res.map(e => (e,accessForm(study,e.id)))))
      }
    )
  }

  def accessAdd(i : Int, e : Int) = check(i, Permission.ADMIN) { study => implicit requet =>
    accessForm(study, e).bindFromRequest.fold(
      form => BadRequest(viewEdit(study)(accessResults = Seq((Identity.get(e),form)))),
      access => {
        access.set
        Redirect(routes.Study.edit(study.id))
      }
    )
  }
}
