package controllers

import site._
import play.api._
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import models._

private[controllers] sealed class VolumeController extends ObjectController[Volume] {
  private[controllers] def action(i : models.Volume.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(models.Volume.get(i)(_), p)

  private[controllers] def Action(i : models.Volume.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(i, p)

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
  protected def citationSet(volume : Volume, cites : Seq[CitationMapping])(implicit site : Site) =
    if (cites.nonEmpty) {
      volume.setCitations(cites.flatMap(c =>
        c._1.map(h => VolumeCitation(volume, h, c._2, c._3))
      ))
    } else macros.Async(false)

  type VolumeFields = (String, Option[String], Seq[CitationMapping])
  type VolumeForm = Form[VolumeFields]
  protected val editForm = Form(tuple(
    "name" -> nonEmptyText,
    "body" -> optional(text),
    "citation" -> seq(citationMapping)
  ))
  protected def editFormFill(v : Volume)(implicit site : Site) =
    v.citations.map { cites =>
      editForm.fill((v.name, v.body, cites.map(citationFill(_))))
    }

  def update(i : models.Volume.Id) = Action(i, Permission.EDIT).async { implicit request =>
    editFormFill(request.obj).flatMap {
      _.bindFromRequest.fold(
        form => ABadForm[VolumeFields](views.html.volume.edit(Right(request.obj), _), form),
        { case (name, body, cites) =>
          request.obj.change(name = name, body = body.flatMap(Maybe(_).opt)).flatMap { _ =>
            citationSet(request.obj, cites).map { _ =>
              if (request.isApi) Ok(request.obj.json)
              else Redirect(request.obj.pageURL)
            }
          }
        }
      )
    }
  }

  protected def ContributeAction(e : Option[models.Party.Id]) =
    Party.Action(e, Some(Permission.CONTRIBUTE)) ~>
      new ActionHandler[Party.Request] {
        protected def handle[A](request : Party.Request[A]) =
          macros.Async(if (request.obj.access < Permission.CONTRIBUTE) Some(Forbidden) else None)
      }

  type AccessForm = Form[(Permission.Value, Permission.Value)]
  protected val accessForm : AccessForm = Form(
    tuple(
      "access" -> Field.enum(Permission),
      "inherit" -> Field.enum(Permission, maxId = Some(Permission.EDIT.id))
    )
  )

  protected def accessFormFill(access : VolumeAccess) : AccessForm =
    accessForm.fill((access.access, access.inherit))

  protected val accessSearchForm = Form(
    "name" -> nonEmptyText
  )

}

object VolumeController extends VolumeController {
  def thumb(v : models.Volume.Id) = Action(v, Permission.VIEW).async { implicit request =>
    request.obj.thumb.flatMap(_.fold(
      Assets.at("/public", "images/draft.png")(request))(
      a => SlotAsset.getFrame(Left(0.25f))(request.withObj(a))))
  }
}

object VolumeHtml extends VolumeController {
  def view(i : models.Volume.Id) = Action(i).async { implicit request =>
    val vol = request.obj
    for {
      _ <- vol.partyAccess
      _ <- vol.toplevelAssets
      _ <- vol.citations
      _ <- vol.summary
      _ <- vol.comments
      _ <- vol.tags
    } yield (Ok(views.html.volume.view()))
  }

  def search = SiteAction.async { implicit request =>
    models.Volume.getAll.flatMap { all =>
      macros.Async.foreach[Volume, SimpleResult](all, vol =>
        for {
          _ <- vol.partyAccess
          _ <- vol.summary
        } yield (()),
        Ok(views.html.volume.search(all, request.queryString.getOrElse("query", Seq("")).head.toString))
      )
    }
  }

  def edit(i : models.Volume.Id) = Action(i, Permission.EDIT).async { implicit request =>
    editFormFill(request.obj).map { form =>
      Ok(views.html.volume.edit(Right(request.obj), form))
    }
  }

  def create(e : Option[models.Party.Id]) = ContributeAction(e) { implicit request =>
    Ok(views.html.volume.edit(Left(request.obj), editForm))
  }

  def add(e : models.Party.Id) = ContributeAction(Some(e)).async { implicit request =>
    editForm.bindFromRequest.fold(
      form => ABadRequest(views.html.volume.edit(Left(request.obj), form)),
      { case (name, body, cites) =>
        for {
          volume <- models.Volume.create(name, body)
          _ <- citationSet(volume, cites)
          _ <- VolumeAccess.set(volume, e, Permission.ADMIN, Permission.CONTRIBUTE)
        } yield (Redirect(volume.pageURL))
      }
    )
  }

  private[this] def viewAdmin(
    status : Status,
    accessChangeForm : Option[(models.Party,AccessForm)] = None,
    accessSearchForm : Form[String] = accessSearchForm,
    accessResults : Seq[(models.Party,AccessForm)] = Seq())(
    implicit request : Request[_]) = {
    val accessChange = accessChangeForm.map(_._1.id)
    request.obj.partyAccess.map { access =>
      val accessForms = access
        .filter(a => accessChange.fold(true)(_ === a.partyId))
        .map(a => (a.party, accessFormFill(a))) ++
        accessChangeForm
      status(views.html.volume.access(request.obj, accessForms, accessSearchForm, accessResults))
    }
  }

  def admin(id : models.Volume.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    viewAdmin(Ok)
  }

  def accessChange(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    models.Party.get(e).flatMap(_.fold(ANotFound) { who =>
    val form = accessForm.bindFromRequest
    form.fold(
      form => viewAdmin(BadRequest, accessChangeForm = Some((who, form))),
      { case (access, inherit) =>
        if (e.unId <= 0 && inherit >= Permission.EDIT)
          viewAdmin(BadRequest, accessChangeForm = Some((who, form.withError("inherit", "access.group.inherit"))))
        else
          VolumeAccess.set(request.obj, e, if (access < inherit) inherit else access, inherit).map { _ =>
            Redirect(routes.VolumeHtml.admin(id))
          }
      }
    )
    })
  }

  def accessDelete(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    (if (!(e === request.identity.id))
      VolumeAccess.delete(request.obj, e)
    else macros.Async(false)).map { _ =>
      Redirect(routes.VolumeHtml.admin(id))
    }
  }

  def accessSearch(id : models.Volume.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    val form = accessSearchForm.bindFromRequest
    form.fold(
      form => viewAdmin(BadRequest, accessSearchForm = form),
      name =>
        models.Party.searchForVolumeAccess(name, request.obj).flatMap { res =>
          viewAdmin(Ok, accessSearchForm = form,
            accessResults = res.map(e => (e, accessForm)))
        }
    )
  }
}

object VolumeApi extends VolumeController {
  def get(i : models.Volume.Id) = Action(i).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }
}
