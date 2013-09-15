package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Volume extends SiteController {

  private[controllers] def check(i : models.Volume.Id, p : Permission.Value = Permission.VIEW)(act : Volume => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Volume.get(i).fold(NotFound : Result) { s =>
      if (s.permission < p)
        Forbidden
      else
        act(s)(request)
    }
  }

  def view(i : models.Volume.Id) = check(i) { volume => implicit request =>
    val top = volume.toplevelAssets
    def group(x : Seq[SlotAsset]) = x.groupBy(_.format.mimeSubTypes._1)
    val (excerpts, files) = top.filter(_.permission >= Permission.DOWNLOAD).partition(_.excerpt)
    Ok(views.html.volume.view(volume, top, group(excerpts), group(files)))
  }

  def listAll = SiteAction { implicit request =>
    Ok(views.html.volume.list(models.Volume.getAll))
  }

  type VolumeForm = Form[(String, Option[String])]
  private[this] val editForm = Form(tuple(
    "name" -> nonEmptyText,
    "body" -> optional(text)
  ))
  private[this] def editFormFill(s : Volume) = editForm.fill((s.name, s.body))

  def edit(i : models.Volume.Id) = check(i, Permission.EDIT) { volume => implicit request =>
    Ok(views.html.volume.edit(Right(volume), editFormFill(volume)))
  }

  def change(i : models.Volume.Id) = check(i, Permission.EDIT) { volume => implicit request =>
    editFormFill(volume).bindFromRequest.fold(
      form => BadRequest(views.html.volume.edit(Right(volume), form)),
      { case (name, body) =>
        volume.change(name = name, body = body.flatMap(maybe(_)))
        Redirect(volume.pageURL)
      }
    )
  }

  def create(e : Option[models.Party.Id]) = UserAction { implicit request =>
    val owner = e.getOrElse(request.identity.id)
    if (request.identity.delegatedBy(owner) < Permission.CONTRIBUTE)
      Forbidden
    else
      Ok(views.html.volume.edit(Left(models.Party.get(owner).get), editForm))
  }

  def add(owner : models.Party.Id) = UserAction { implicit request =>
    if (request.identity.delegatedBy(owner) < Permission.CONTRIBUTE)
      Forbidden
    else
      editForm.bindFromRequest.fold(
        form => BadRequest(views.html.volume.edit(Left(models.Party.get(owner).get), form)),
        { case (name, body) =>
          val volume = models.Volume.create(name, body)
          VolumeAccess(volume.id, owner, Permission.ADMIN, Permission.CONTRIBUTE).set
          Redirect(volume.pageURL)
        }
      )
  }

  type AccessForm = Form[VolumeAccess]
  private[this] def accessForm(volume : Volume, party : models.Party.Id) : AccessForm = Form(
    mapping(
      "access" -> number(min=0, max=Permission.maxId-1),
      "inherit" -> number(min=0, max=(if (party.unId > 0) Permission.EDIT else Permission.DOWNLOAD).id)
    )((access, inherit) => VolumeAccess(
      volume.id, party, 
      Permission(access.max(inherit)),
      Permission(inherit)
    ))(a =>
      if (a.volumeId == volume.id && a.partyId == party)
        Some((a.access.id, a.inherit.id))
      else
        None
    )
  )

  private[this] val accessSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[this] def viewAdmin(volume : Volume)(
    accessChangeForm : Option[(models.Party,AccessForm)] = None,
    accessSearchForm : Form[String] = accessSearchForm,
    accessResults : Seq[(models.Party,AccessForm)] = Seq())(
    implicit request : SiteRequest[_]) = {
    val accessChange = accessChangeForm.map(_._1.id)
    val accessForms = volume.partyAccess().filter(a => Some(a.partyId) != accessChange).map(a => (a.party, accessForm(volume, a.partyId).fill(a))) ++ accessChangeForm
    views.html.volume.admin(volume, accessForms, accessSearchForm, accessResults)
  }

  def admin(i : models.Volume.Id) = check(i, Permission.ADMIN) { volume => implicit request =>
    Ok(viewAdmin(volume)())
  }

  def accessChange(i : models.Volume.Id, e : models.Party.Id) = check(i, Permission.ADMIN) { volume => implicit request =>
    accessForm(volume, e).bindFromRequest.fold(
      form => BadRequest(viewAdmin(volume)(accessChangeForm = Some((models.Party.get(e).get, form)))),
      access => {
        access.set
        Redirect(routes.Volume.admin(volume.id))
      }
    )
  }

  def accessDelete(i : models.Volume.Id, e : models.Party.Id) = check(i, Permission.ADMIN) { volume => implicit request =>
    if (e != request.identity.id)
      VolumeAccess.delete(volume.id, e)
    Redirect(routes.Volume.admin(volume.id))
  }

  def accessSearch(i : models.Volume.Id) = check(i, Permission.ADMIN) { volume => implicit request =>
    val form = accessSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(volume)(accessSearchForm = form)),
      name => {
        val res = models.Party.searchForVolumeAccess(name, volume.id)
        Ok(viewAdmin(volume)(accessSearchForm = form, 
          accessResults = res.map(e => (e,accessForm(volume,e.id)))))
      }
    )
  }

  def accessAdd(i : models.Volume.Id, e : models.Party.Id) = check(i, Permission.ADMIN) { volume => implicit request =>
    accessForm(volume, e).bindFromRequest.fold(
      form => BadRequest(viewAdmin(volume)(accessResults = Seq((models.Party.get(e).get, form)))),
      access => {
        access.set
        Redirect(routes.Volume.admin(volume.id))
      }
    )
  }
}
