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
import models._

object Study extends Controller {

  private[this] def check(i : Int, p : Permission.Value)(act : (Study, Permission.Value) => SiteRequest[AnyContent] => Result) = SiteAction { request =>
    val a = StudyAccess.check(request.identity.id, i)
    if (a < p)
      Forbidden
    else
      act(models.Study.get(i).get, a)(request)
  }

  def view(i : Int) = check(i, Permission.VIEW) { (study, access) => implicit request =>
    Ok(views.html.study(study, access))
  }

  /* list of studies belonging to entity e and viewable by the current identity
   * poorly named, and should go elsewhere/be generalized to other searches */
  def viewable(e : Int)(implicit request : SiteRequest[_]) = DB.withSession { implicit session =>
    val l = for { 
      a <- StudyAccess.byEntity(e, Permission.CONTRIBUTE).sortBy(_.access.desc)
      if StudyAccess.filterForEntity(request.identity.id)(a.studyId)
      s <- a.study
    } yield (s)
    l.list
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

  private[this] def viewEdit(study : Study, access : Permission.Value)(
    editForm : Form[(String,String)] = editFormFill(study),
    accessChangeForm : Option[(Entity,Form[StudyAccess])] = None,
    accessSearchForm : Form[String] = accessSearchForm,
    accessResults : Seq[(Entity,Form[StudyAccess])] = Seq())(
    implicit request : SiteRequest[_]) = {
    val accessChange = accessChangeForm.map(_._1.id)
    val accessForms = study.access().filter(a => Some(a.entityId) != accessChange).map(a => (a.entity, accessForm(study, a.entityId).fill(a))) ++ accessChangeForm
    views.html.studyEdit(study, access, editForm, accessForms, accessSearchForm, accessResults)
  }

  def edit(i : Int) = check(i, Permission.EDIT) { (study, access) => implicit request =>
    Ok(viewEdit(study, access)())
  }

  def change(i : Int) = check(i, Permission.EDIT) { (study, access) => implicit request =>
    editFormFill(study).bindFromRequest.fold(
      form => BadRequest(viewEdit(study, access)(editForm = form)),
      { case (title, description) =>
        study.title = title
        study.description = maybe(description)
        study.commit
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def accessChange(i : Int, e : Int) = check(i, Permission.ADMIN) { (study, perm) => implicit request =>
    accessForm(study, e).bindFromRequest.fold(
      form => BadRequest(viewEdit(study, perm)(accessChangeForm = Some((models.Entity.get(e), form)))),
      access => {
        access.commit
        Redirect(routes.Study.edit(study.id))
      }
    )
  }

  def accessDelete(i : Int, e : Int) = check(i, Permission.ADMIN) { (study, perm) => implicit request =>
    if (request.identity.id != e)
      StudyAccess.delete(study.id, e)
    Redirect(routes.Study.edit(study.id))
  }

  def accessSearch(i : Int) = check(i, Permission.ADMIN) { (study, perm) => implicit request =>
    val form = accessSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewEdit(study, perm)(accessSearchForm = form)),
      name => {
        val res = DB.withSession { implicit session =>
          models.Entity.byName(name).filter(_.id.notIn(StudyAccess.byStudy(study.id).map(_.entityId))).take(8).list
        }
        Ok(viewEdit(study, perm)(accessSearchForm = form, 
          accessResults = res.map(e => (e,accessForm(study,e.id)))))
      }
    )
  }

  def accessAdd(i : Int, e : Int) = check(i, Permission.ADMIN) { (study, perm) => implicit requet =>
    accessForm(study, e).bindFromRequest.fold(
      form => BadRequest(viewEdit(study, perm)(accessResults = Seq((models.Entity.get(e),form)))),
      access => {
        access.add
        Redirect(routes.Study.edit(study.id))
      }
    )
  }
}
