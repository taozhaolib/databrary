package controllers

import scala.concurrent.Future
import play.api._
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import site._
import macros._
import models._

private[controllers] sealed class VolumeController extends ObjectController[Volume] {
  private[controllers] def action(i : models.Volume.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(models.Volume.get(i)(_), p)

  private[controllers] def Action(i : models.Volume.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(i, p)

  protected def searchResults(implicit request : SiteRequest[_]) : (VolumeController.SearchForm, Future[Seq[Volume]]) = {
    val form = new VolumeController.SearchForm
    form._bind
    (form, Volume.search(form.query.value, form.party.value))
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
  protected def citationFill(cite : VolumeCitation) = (Some(cite.head), cite.url, cite.body)
  private def citationSet(volume : Volume, cites : Seq[CitationMapping])(implicit site : Site) =
    if (cites.nonEmpty) {
      volume.setCitations(cites.flatMap(c =>
        c._1.map(h => VolumeCitation(volume, h, c._2, c._3))
      ))
    } else macros.Async(false)

  protected def editFormFill(implicit request : Request[_]) =
    request.obj.citations.map(new VolumeController.EditForm(_))

  def update(i : models.Volume.Id) = Action(i, Permission.EDIT).async { implicit request =>
    val vol = request.obj
    for {
      form <- editFormFill
      _ = form._bind
      _ <- vol.change(name = form.name.value, body = form.body.value.map(Maybe(_).opt))
      _ <- citationSet(vol, form.citation.value)
    } yield (result(vol))
  }

  def create(owner : models.Party.Id) = ContributeAction(Some(owner)).async { implicit request =>
    val form = new VolumeController.CreateForm
    form._bind
    for {
      vol <- models.Volume.create(form.name.value.get, form.body.value.flatMap(Maybe(_).opt))
      _ <- citationSet(vol, form.citation.value)
      _ <- VolumeAccess.set(vol, owner, Permission.ADMIN, Permission.CONTRIBUTE)
    } yield (result(vol))
  }

  protected def ContributeAction(e : Option[models.Party.Id]) =
    PartyController.Action(e, Some(Permission.CONTRIBUTE)) ~>
      new ActionHandler[PartyController.Request] {
        protected def handle[A](request : PartyController.Request[A]) =
	  request.obj.party.access.map(a => if (a.group < Permission.CONTRIBUTE) Some(Forbidden) else None)
      }

  type AccessMapping = (Permission.Value, Permission.Value)
  type AccessForm = Form[AccessMapping]
  protected val accessForm : AccessForm = Form(
    tuple(
      ("access", Field.enum(Permission)),
      ("inherit", Field.enum(Permission, maxId = Some(Permission.EDIT.id)))
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
  class SearchForm(implicit request : SiteRequest[_]) extends HtmlForm[SearchForm](routes.VolumeHtml.search, views.html.volume.search(Nil, _)) {
    val query = Field(optional(nonEmptyText))
    val party = Field(OptionMapping(of[Party.Id]))
  }

  trait VolumeForm extends FormView[VolumeForm] {
    def actionName : String
    def formName : String = actionName + " Volume"

    val name : Field[Option[String]]
    val body = Field(OptionMapping(text))
    val citation = Field(seq(citationMapping))
  }

  class EditForm(cites : Seq[VolumeCitation])(implicit request : Request[_])
    extends HtmlForm[EditForm](routes.VolumeHtml.update(request.obj.id), views.html.volume.edit(_)) with VolumeForm {
    def actionName = "Change"
    override def formName = "Edit Volume"
    val name = Field(OptionMapping(nonEmptyText)).init(Some(request.obj.name))
    body.init(Some(request.obj.body.getOrElse("")))
    citation.init(cites.map(citationFill(_)))
    _fill
  }

  class CreateForm(implicit request : PartyController.Request[_])
    extends HtmlForm[CreateForm](routes.VolumeHtml.create(request.obj.party.id), views.html.volume.edit(_)) with VolumeForm {
    def actionName = "Create"
    val name = Field(nonEmptyText.transform[Option[String]](Some(_), _.getOrElse("")))
  }

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
      access <- vol.partyAccess()
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
    val (form, res) = searchResults
    for {
      vl <- res
      vols <- macros.Async.map[Volume, (Volume, Seq[Party]), Seq[(Volume, Seq[Party])]](vl, vol => for {
	access <- vol.partyAccess(Permission.ADMIN)
      } yield ((vol, access.map(_.party))))
    } yield (Ok(views.html.volume.search(vols, form)))
  }

  def edit(i : models.Volume.Id) = Action(i, Permission.EDIT).async { implicit request =>
    editFormFill.flatMap(_.Ok)
  }

  def add(e : Option[models.Party.Id]) = ContributeAction(e).async { implicit request =>
    (new VolumeController.CreateForm).Ok
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
    request.obj.partyAccess().map { access =>
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
  def get(i : models.Volume.Id) = Action(i).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  private final val queryOpts : JsonOptions.Options = Map("summary" -> Nil, "access" -> Seq("ADMIN"))
  def query = SiteAction.async { implicit request =>
    for {
      vl <- searchResults._2
      vols <- macros.Async.map[Volume, JsonRecord, Seq[JsonRecord]](vl, _.json(queryOpts))
    } yield (Ok(JsonArray(vols)))
  }

  def accessGet(volumeId : models.Volume.Id) = Action(volumeId, Permission.ADMIN).async { implicit request =>
    for {
      parents <- request.obj.partyAccess()
    } yield (Ok(JsonRecord.map[VolumeAccess](a =>
      JsonRecord(a.partyId) ++ (a.json - "volume"))(parents)))
  }

  def accessSearch(volumeId : models.Volume.Id, name : String) = Action(volumeId, Permission.ADMIN).async { implicit request =>
    models.Party.searchForVolumeAccess(name, request.obj)
      .map(r => Ok(JsonRecord.map[Party](_.json)(r)))
  }

}
