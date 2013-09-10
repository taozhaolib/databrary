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

  private[controllers] def check(i : models.Study.Id, p : Permission.Value = Permission.VIEW)(act : Study => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Study.get(i).fold(NotFound : Result) { s =>
      if (s.permission < p)
        Forbidden
      else
        act(s)(request)
    }
  }

  def view(i : models.Study.Id) = check(i) { study => implicit request =>
    def group(x : Seq[AssetLink]) = x.groupBy(_.asset(request.db).format.mimeSubTypes._1)
    val (excerpts, files) = study.assets(request.db).filter(_.permission >= Permission.DOWNLOAD).partition(_.asset(request.db).excerpt)
    Ok(views.html.study.view(study, (group(excerpts), group(files))))
  }

  def listAll = SiteAction { implicit request =>
    Ok(views.html.study.list(models.Study.getAll))
  }

  type StudyForm = Form[(String, Option[String])]
  private[this] val editForm = Form(tuple(
    "title" -> nonEmptyText,
    "description" -> optional(text)
  ))
  private[this] def editFormFill(s : Study) = editForm.fill((s.title, s.description))

  def edit(i : models.Study.Id) = check(i, Permission.EDIT) { study => implicit request =>
    Ok(views.html.study.edit(study, editFormFill(study)))
  }

  def change(i : models.Study.Id) = check(i, Permission.EDIT) { study => implicit request =>
    editFormFill(study).bindFromRequest.fold(
      form => BadRequest(views.html.study.edit(study, form)),
      { case (title, description) =>
        study.change(title = title, description = description.flatMap(maybe(_)))
        Redirect(study.pageURL)
      }
    )
  }

  type AccessForm = Form[StudyAccess]
  private[this] def accessForm(study : Study, party : models.Party.Id) : AccessForm = Form(
    mapping(
      "access" -> number(min=0, max=Permission.maxId-1),
      "inherit" -> number(min=0, max=(if (party.unId > 0) Permission.EDIT else Permission.DOWNLOAD).id)
    )((access, inherit) => StudyAccess(
      study.id, party, 
      Permission(access.max(inherit)),
      Permission(inherit)
    ))(a =>
      if (a.studyId == study.id && a.partyId == party)
        Some((a.access.id, a.inherit.id))
      else
        None
    )
  )

  private[this] val accessSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewAdmin(study : Study)(
    accessChangeForm : Option[(models.Party,AccessForm)] = None,
    accessSearchForm : Form[String] = accessSearchForm,
    accessResults : Seq[(models.Party,AccessForm)] = Seq())(
    implicit request : SiteRequest[_]) = {
    val accessChange = accessChangeForm.map(_._1.id)
    val accessForms = study.partyAccess().filter(a => Some(a.partyId) != accessChange).map(a => (a.party, accessForm(study, a.partyId).fill(a))) ++ accessChangeForm
    views.html.study.admin(study, accessForms, accessSearchForm, accessResults)
  }

  def admin(i : models.Study.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    Ok(viewAdmin(study)())
  }

  def accessChange(i : models.Study.Id, e : models.Party.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    accessForm(study, e).bindFromRequest.fold(
      form => BadRequest(viewAdmin(study)(accessChangeForm = Some((models.Party.get(e).get, form)))),
      access => {
        access.set
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def accessDelete(i : models.Study.Id, e : models.Party.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    if (e != request.identity.id)
      StudyAccess.delete(study.id, e)
    Redirect(routes.Study.edit(study.id))
  }

  def accessSearch(i : models.Study.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    val form = accessSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(study)(accessSearchForm = form)),
      name => {
        val res = models.Party.searchForStudyAccess(name, study.id)
        Ok(viewAdmin(study)(accessSearchForm = form, 
          accessResults = res.map(e => (e,accessForm(study,e.id)))))
      }
    )
  }

  def accessAdd(i : models.Study.Id, e : models.Party.Id) = check(i, Permission.ADMIN) { study => implicit request =>
    accessForm(study, e).bindFromRequest.fold(
      form => BadRequest(viewAdmin(study)(accessResults = Seq((models.Party.get(e).get, form)))),
      access => {
        access.set
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def create(e : Option[models.Party.Id]) = UserAction { implicit request =>
    val owner = e.getOrElse(request.identity.id)
    if (request.identity.delegatedBy(owner) < Permission.CONTRIBUTE)
      Forbidden
    else {
      val form = editForm.bindFromRequest
      val study = (models.Study.create _).tupled(form.value.getOrElse(("New study", None)))
      StudyAccess(study.id, owner, Permission.ADMIN, Permission.CONTRIBUTE).set
      Created(views.html.study.edit(study, form))
    }
  }
}
