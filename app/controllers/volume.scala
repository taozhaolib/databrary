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
  private def citationSet(volume : Volume, cites : Seq[CitationMapping])(implicit site : Site) =
    if (cites.nonEmpty) {
      volume.setCitations(cites.flatMap(c =>
        c._1.map(h => VolumeCitation(volume, h, c._2, c._3))
      ))
    } else macros.Async(false)

  type VolumeMapping = (Option[String], Option[String], Seq[CitationMapping])
  type VolumeForm = Form[VolumeMapping]
  protected val editForm = Form(tuple(
    "name" -> OptionMapping(nonEmptyText),
    "body" -> OptionMapping(text),
    "citation" -> seq(citationMapping)
  ))
  protected def editFormFill(v : Volume)(implicit site : Site) =
    v.citations.map { cites =>
      editForm.fill((Some(v.name), Some(v.body.getOrElse("")), cites.map(citationFill(_))))
    }

  def update(i : models.Volume.Id) = Action(i, Permission.EDIT).async { implicit request =>
    val vol = request.obj
    def bad(form : VolumeForm) =
      ABadForm[VolumeMapping](views.html.volume.edit(Right(vol), _), form)
    editFormFill(vol).flatMap {
      _.bindFromRequest.fold(bad _, {
        case (name, body, cites) =>
          for {
            _ <- vol.change(name = name, body = body.map(Maybe(_).opt))
            _ <- citationSet(vol, cites)
          } yield (result(vol))
        }
      )
    }
  }

  def create(owner : models.Party.Id) = ContributeAction(Some(owner)).async { implicit request =>
    def bad(form : VolumeForm) =
      ABadForm[VolumeMapping](views.html.volume.edit(Left(request.obj), _), form)
    val form = editForm.bindFromRequest
    form.fold(bad _, {
      case (None, _, _) => bad(form.withError("name", "error.required"))
      case (Some(name), body, cites) =>
        for {
          vol <- models.Volume.create(name, body.flatMap(Maybe(_).opt))
          _ <- citationSet(vol, cites)
          _ <- VolumeAccess.set(vol, owner, Permission.ADMIN, Permission.CONTRIBUTE)
        } yield (result(vol))
    })
  }

  protected def ContributeAction(e : Option[models.Party.Id]) =
    PartyController.Action(e, Some(Permission.CONTRIBUTE)) ~>
      new ActionHandler[PartyController.Request] {
        protected def handle[A](request : PartyController.Request[A]) =
          macros.Async(if (request.obj.access < Permission.CONTRIBUTE) Some(Forbidden) else None)
      }

  type AccessMapping = (Permission.Value, Permission.Value)
  type AccessForm = Form[AccessMapping]
  protected val accessForm : AccessForm = Form(
    tuple(
      "access" -> Field.enum(Permission),
      "inherit" -> Field.enum(Permission, maxId = Some(Permission.EDIT.id))
    )
  )

  protected def accessFormFill(access : VolumeAccess) : AccessForm =
    accessForm.fill((access.access, access.inherit))

  def accessChange(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    models.Party.get(e).flatMap(_.fold(ANotFound) { who =>
    val form = accessForm.bindFromRequest
    def bad(form : AccessForm) =
      AbadForm[AccessMapping](f => VolumeHtml.viewAdmin(accessChangeForm = Some((who, f))), form)
    form.fold(bad _, {
      case (access, inherit) =>
        if (e.unId <= 0 && inherit >= Permission.EDIT)
          bad(form.withError("inherit", "access.group.inherit"))
        else
          VolumeAccess.set(request.obj, e, if (access < inherit) inherit else access, inherit).map { _ =>
            result(request.obj)
          }
      }
    )
    })
  }

  def accessDelete(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    (if (!(e === request.identity.id))
      VolumeAccess.delete(request.obj, e)
    else macros.Async(false)).map { _ =>
      result(request.obj)
    }
  }

}

object VolumeController extends VolumeController {
  def thumb(v : models.Volume.Id) = Action(v, Permission.VIEW).async { implicit request =>
    request.obj.thumb.flatMap(_.fold(
      Assets.at("/public", "images/draft.png")(request))(
      a => SlotAssetController.getFrame(Left(0.25f))(request.withObj(a))))
  }
}

object VolumeHtml extends VolumeController {
  def view(i : models.Volume.Id) = Action(i).async { implicit request =>
    val vol = request.obj
    for {
      summary <- vol.summary
      access <- vol.partyAccess
      top <- vol.top
      sessions <- vol.sessions
      records <- vol.recordCategorySlots
      assets <- vol.toplevelAssets
      citations <- vol.citations
      comments <- vol.comments
      tags <- vol.tags
    } yield (Ok(views.html.volume.view(summary, access, top, sessions, records, assets, citations, comments, tags)))
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

  def add(e : Option[models.Party.Id]) = ContributeAction(e) { implicit request =>
    Ok(views.html.volume.edit(Left(request.obj), editForm))
  }

  protected val accessSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[controllers] def viewAdmin(
    accessChangeForm : Option[(models.Party,AccessForm)] = None,
    accessSearchForm : Form[String] = accessSearchForm,
    accessResults : Seq[(models.Party,AccessForm)] = Seq())(
    implicit request : Request[_]) = {
    val accessChange = accessChangeForm.map(_._1.id)
    request.obj.partyAccess.map { access =>
      val accessForms = access
        .filter(a => accessChange.forall(_ === a.partyId))
        .map(a => (a.party, accessFormFill(a))) ++
        accessChangeForm
      views.html.volume.access(request.obj, accessForms, accessSearchForm, accessResults)
    }
  }

  def admin(id : models.Volume.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    viewAdmin().map(Ok(_))
  }

  def accessSearch(id : models.Volume.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    val form = accessSearchForm.bindFromRequest
    form.fold(
      AbadForm[String](f => viewAdmin(accessSearchForm = f), _),
      name =>
        models.Party.searchForVolumeAccess(name, request.obj).flatMap { res =>
          viewAdmin(accessSearchForm = form,
            accessResults = res.map(e => (e, accessForm)))
            .map(Ok(_))
        }
    )
  }
}

object VolumeApi extends VolumeController {
  def query(query : String = "") = SiteAction.async { implicit request =>
    Maybe(query).opt.fold(Volume.getAll)(Volume.search(_))
      .map(l => Ok(JsonRecord.map[Volume](_.json)(l)))
  }

  def get(i : models.Volume.Id) = Action(i).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  def accessGet(volumeId : models.Volume.Id) = Action(volumeId, Permission.ADMIN).async { implicit request =>
    for {
      parents <- request.obj.partyAccess
    } yield (Ok(JsonRecord.map[VolumeAccess](a =>
      JsonRecord(a.partyId) ++ (a.json - "volume"))(parents)))
  }

  def accessSearch(volumeId : models.Volume.Id, name : String) = Action(volumeId, Permission.ADMIN).async { implicit request =>
    models.Party.searchForVolumeAccess(name, request.obj)
      .map(r => Ok(JsonRecord.map[Party](_.json)(r)))
  }

}
