package controllers

import site._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Volume extends SiteController {
  type Request[A] = RequestObject[Volume]#Site[A]

  private[controllers] def action(i : models.Volume.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(models.Volume.get(i)(_), p)

  private[controllers] def Action(i : models.Volume.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(i, p)

  def view(i : models.Volume.Id) = Action(i) { implicit request =>
    val top = request.obj.toplevelAssets
    def group(x : Seq[SlotAsset]) = x.groupBy(_.format.mimeSubTypes._1)
    val (excerpts, files) = top.filter(_.permission >= Permission.DOWNLOAD).partition(_.excerpt)
    Ok(views.html.volume.view(request.obj, top, group(excerpts), group(files)))
  }

  def listAll = SiteAction { implicit request =>
    Ok(views.html.volume.list(models.Volume.getAll))
  }

  type CitationMapping = (Option[String], Option[String], Option[String])
  private val citationMapping = tuple(
    "head" -> optional(nonEmptyText),
    "url" -> optional(nonEmptyText),
    "body" -> optional(nonEmptyText)
  ).verifying(Messages("citation.invalid"), _ match {
    case (Some(head), url, body) => true // TODO: validate URL
    case (None, None, None) => true
    case _ => false
  })
  private def citationFill(cite : VolumeCitation) = (Some(cite.head), cite.url, cite.body)
  private def citationSet(volume : Volume, cites : Seq[CitationMapping])(implicit site : Site) =
    if (cites.nonEmpty) {
      VolumeCitation.setVolume(volume, cites.flatMap(c =>
        c._1.map(h => VolumeCitation(volume, h, c._2, c._3))
      ))
    }

  type VolumeForm = Form[(String, Option[String], Seq[CitationMapping])]
  private[this] val editForm = Form(tuple(
    "name" -> nonEmptyText,
    "body" -> optional(text),
    "citation" -> seq(citationMapping)
  ))
  private[this] def editFormFill(v : Volume)(implicit site : Site) = editForm.fill((v.name, v.body, v.citations.map(citationFill(_)) :+ ((Some(""), None, None))))

  def edit(i : models.Volume.Id) = Action(i, Permission.EDIT) { implicit request =>
    Ok(views.html.volume.edit(Right(request.obj), editFormFill(request.obj)))
  }

  def change(i : models.Volume.Id) = Action(i, Permission.EDIT) { implicit request =>
    editFormFill(request.obj).bindFromRequest.fold(
      form => BadRequest(views.html.volume.edit(Right(request.obj), form)),
      { case (name, body, cites) =>
        request.obj.change(name = name, body = body.flatMap(maybe(_)))
        citationSet(request.obj, cites)
        Redirect(request.obj.pageURL)
      }
    )
  }

  def create(e : Option[models.Party.Id]) = SiteAction.auth { implicit request =>
    e.fold[Option[Party]](Some(request.identity))(models.Party.get(_)).fold[SimpleResult](NotFound) { owner =>
      if (owner.access < Permission.CONTRIBUTE || request.identity.delegatedBy(owner.id) < Permission.CONTRIBUTE)
        Forbidden
      else
        Ok(views.html.volume.edit(Left(owner), editForm))
    }
  }

  def add(e : models.Party.Id) = SiteAction.auth { implicit request =>
    models.Party.get(e).fold[SimpleResult](NotFound) { owner =>
      if (owner.access < Permission.CONTRIBUTE || request.identity.delegatedBy(owner.id) < Permission.CONTRIBUTE)
        Forbidden
      else
        editForm.bindFromRequest.fold(
          form => BadRequest(views.html.volume.edit(Left(owner), form)),
          { case (name, body, cites) =>
            val volume = models.Volume.create(name, body)
            citationSet(volume, cites)
            VolumeAccess(volume, owner.id, Permission.ADMIN, Permission.CONTRIBUTE).set
            Redirect(volume.pageURL)
          }
        )
    }
  }

  type AccessForm = Form[VolumeAccess]
  private[this] def accessForm(volume : Volume, party : models.Party.Id) : AccessForm = Form(
    mapping(
      "access" -> number(min=0, max=Permission.maxId-1),
      "inherit" -> number(min=0, max=(if (party.unId > 0) Permission.EDIT else Permission.DOWNLOAD).id)
    )((access, inherit) => VolumeAccess(
      volume, party, 
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

  private[this] def viewAdmin(
    accessChangeForm : Option[(models.Party,AccessForm)] = None,
    accessSearchForm : Form[String] = accessSearchForm,
    accessResults : Seq[(models.Party,AccessForm)] = Seq())(
    implicit request : Request[_]) = {
    val accessChange = accessChangeForm.map(_._1.id)
    val accessForms = request.obj.partyAccess.filter(a => Some(a.partyId) != accessChange).map(a => (a.party, accessForm(request.obj, a.partyId).fill(a))) ++ accessChangeForm
    views.html.volume.access(request.obj, accessForms, accessSearchForm, accessResults)
  }

  def admin(id : models.Volume.Id) = Action(id, Permission.ADMIN) { implicit request =>
    Ok(viewAdmin())
  }

  def accessChange(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN) { implicit request =>
    accessForm(request.obj, e).bindFromRequest.fold(
      form => BadRequest(viewAdmin(accessChangeForm = Some((models.Party.get(e).get, form)))),
      access => {
        access.set
        Redirect(routes.Volume.admin(id))
      }
    )
  }

  def accessDelete(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN) { implicit request =>
    if (e != request.identity.id)
      VolumeAccess.delete(id, e)
    Redirect(routes.Volume.admin(id))
  }

  def accessSearch(id : models.Volume.Id) = Action(id, Permission.ADMIN) { implicit request =>
    val form = accessSearchForm.bindFromRequest
    form.fold(
      form => BadRequest(viewAdmin(accessSearchForm = form)),
      name => {
        val res = models.Party.searchForVolumeAccess(name, id)
        Ok(viewAdmin(accessSearchForm = form, 
          accessResults = res.map(e => (e,accessForm(request.obj, e.id)))))
      }
    )
  }

  def accessAdd(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN) { implicit request =>
    accessForm(request.obj, e).bindFromRequest.fold(
      form => BadRequest(viewAdmin(accessResults = Seq((models.Party.get(e).get, form)))),
      access => {
        access.set
        Redirect(routes.Volume.admin(id))
      }
    )
  }
}
