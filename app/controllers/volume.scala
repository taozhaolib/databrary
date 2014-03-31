package controllers

import scala.concurrent.Future
import play.api._
import          mvc._
import          data._
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

  protected def searchResults(implicit request : SiteRequest[AnyContent]) : (VolumeController.SearchForm, Future[Seq[Volume]]) = {
    val form = new VolumeController.SearchForm()._bind
    (form, Volume.search(form.query.get, form.party.get))
  }

  type CitationMapping = (Option[String], Option[String], Option[String])
  private val citationMapping = Forms.tuple(
    "head" -> Forms.optional(Forms.nonEmptyText),
    "url" -> Forms.optional(Forms.nonEmptyText),
    "body" -> Forms.optional(Forms.nonEmptyText)
  ).verifying("citation.invalid", _ match {
    case (Some(head), url, body) => true // TODO: validate URL
    case (None, None, None) => true
    case _ => false
  })
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
      _ <- vol.change(name = form.name.get,
	alias = form.alias.get.map(Maybe(_).opt),
	body = form.body.get.map(Maybe(_).opt))
      _ <- citationSet(vol, form.citation.get)
    } yield (result(vol))
  }

  def create(owner : models.Party.Id) = ContributeAction(Some(owner)).async { implicit request =>
    val form = new VolumeController.CreateForm()._bind
    for {
      vol <- models.Volume.create(form.name.get.get, form.alias.get.flatMap(Maybe(_).opt), form.body.get.flatMap(Maybe(_).opt))
      _ <- citationSet(vol, form.citation.get)
      _ <- VolumeAccess.set(vol, owner, Permission.ADMIN, Permission.CONTRIBUTE)
    } yield (result(vol))
  }

  protected def ContributeAction(e : Option[models.Party.Id]) =
    PartyController.Action(e, Some(Permission.CONTRIBUTE)) ~>
      new ActionHandler[PartyController.Request] {
        protected def handle[A](request : PartyController.Request[A]) =
	  request.obj.party.access.map(a => if (a.group < Permission.CONTRIBUTE) Some(Forbidden) else None)
      }

  protected def accessForm(access : VolumeAccess)(implicit request : Request[_]) =
    new VolumeController.AccessForm(access.party)._fill(access)

  def accessChange(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    for {
      who <- models.Party.get(e).map(_.getOrElse(throw NotFoundException))
      form = new VolumeController.AccessForm(who)._bind
      _ <- if (form.delete.get)
	  VolumeAccess.delete(request.obj, e)
	else
	  VolumeAccess.set(request.obj, e, access = max(form.access.get, form.inherit.get), inherit = form.inherit.get, funding = Maybe(form.funding.get).opt)
    } yield (result(request.obj))
  }

  def accessSearch(id : models.Volume.Id) =
    Action(id, Permission.ADMIN).async { implicit request =>
      val form = new VolumeController.AccessSearchForm()._bind
      for {
	res <- models.Party.searchForVolumeAccess(form.name.get, request.obj)
	r <- if (request.isApi) macros.Async(Ok(JsonRecord.map[Party](_.json)(res)))
	  else VolumeHtml.viewAdmin(accessSearchForm = Some(form), accessResults = res)
	    .map(Ok(_))
      } yield (r)
    }
}

object VolumeController extends VolumeController {
  final class SearchForm(implicit request : SiteRequest[_])
    extends HtmlForm[SearchForm](
      routes.VolumeHtml.search,
      views.html.volume.search(Nil, _)) {
    val query = Field(Forms.optional(Forms.nonEmptyText))
    val party = Field(OptionMapping(Forms.of[Party.Id]))
  }

  trait VolumeForm extends FormView {
    def actionName : String
    def formName : String = actionName + " Volume"

    val name : Field[Option[String]]
    val alias = Field(OptionMapping(Forms.text(maxLength = 64)))
    val body = Field(OptionMapping(Forms.text))
    val citation = Field(Forms.seq(citationMapping))
  }

  private def citationFill(cite : VolumeCitation) = (Some(cite.head), cite.url, cite.body)
  final class EditForm(cites : Seq[VolumeCitation])(implicit request : Request[_])
    extends HtmlForm[EditForm](
      routes.VolumeHtml.update(request.obj.id),
      views.html.volume.edit(_)) with VolumeForm {
    def actionName = "Update"
    override def formName = "Edit Volume"
    val name = Field(OptionMapping(Forms.nonEmptyText)).fill(Some(request.obj.name))
    body.fill(Some(request.obj.body.getOrElse("")))
    alias.fill(Some(request.obj.alias.getOrElse("")))
    citation.fill(cites.map(citationFill(_)) :+ ((Some(""), None, None)))
  }

  final class CreateForm(implicit request : PartyController.Request[_])
    extends HtmlForm[CreateForm](
      routes.VolumeHtml.create(request.obj.party.id),
      views.html.volume.edit(_)) with VolumeForm {
    def actionName = "Create"
    val name = Field(Mappings.some(Forms.nonEmptyText))
  }

  final class AccessForm(val party : Party)(implicit request : Request[_])
    extends AHtmlForm[AccessForm](
      routes.VolumeHtml.accessChange(request.obj.id, party.id),
      f => VolumeHtml.viewAdmin(accessChangeForm = Some(f))) {
    def partyId = party.id
    val access = Field(Mappings.enum(Permission, maxId = Some(if (party.id.unId <= 0) Permission.DOWNLOAD.id else Permission.ADMIN.id)))
    val inherit = Field(Mappings.enum(Permission, maxId = Some(if (party.id.unId <= 0) Permission.DOWNLOAD.id else Permission.EDIT.id)))
    val funding = Field(Forms.text)
    val delete = Field(if (request.identity === party) Forms.boolean.verifying("access.delete.self", !_) else Forms.boolean).fill(false)
    private[controllers] def _fill(a : VolumeAccess) : this.type = {
      assert(a.party === party)
      access.fill(a.access)
      inherit.fill(a.inherit)
      funding.fill(a.funding.getOrElse(""))
      this
    }
  }

  final class AccessSearchForm(implicit request : Request[_])
    extends AHtmlForm[AccessSearchForm](
      routes.VolumeHtml.accessSearch(request.obj.id),
      f => VolumeHtml.viewAdmin(accessSearchForm = Some(f))) {
    val name = Field(Forms.nonEmptyText)
  }

  def thumb(v : models.Volume.Id) = Action(v, Permission.VIEW).async { implicit request =>
    request.obj.thumb.flatMap(_.fold(
      Assets.at("/public", "images/draft.png")(request))(
      a => SlotAssetController.getFrame(Left(0.25f))(request.withObj(a))))
  }
}

object VolumeHtml extends VolumeController with HtmlController {
  import VolumeController._

  def view(i : models.Volume.Id) = Action(i).async { implicit request =>
    val vol = request.obj
    for {
      summary <- vol.summary
      access <- vol.partyAccess()
      top <- vol.top
      sessions <- vol.sessions
      records <- vol.recordCategorySlots
      excerpts <- vol.excerpts
      citations <- vol.citations
      comments <- vol.comments
      tags <- vol.tags
    } yield (Ok(views.html.volume.view(summary, access, top, sessions, records, excerpts, citations, comments, tags)))
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
    (new CreateForm).Ok
  }

  private[controllers] def viewAdmin(
    accessChangeForm : Option[AccessForm] = None,
    accessSearchForm : Option[AccessSearchForm] = None,
    accessResults : Seq[Party] = Nil)(
    implicit request : Request[_]) = {
    val change = accessChangeForm.map(_.partyId.unId).toSet
    for {
      access <- request.obj.partyAccess()
      forms = access
        .filterNot(a => change.contains(a.partyId.unId))
        .map(accessForm(_)) ++
	accessChangeForm
      results = accessResults.map(new AccessForm(_))
    } yield (views.html.volume.access(request.obj, forms, accessSearchForm.getOrElse(new AccessSearchForm), results))
  }

  def admin(id : models.Volume.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    viewAdmin().map(Ok(_))
  }
}

object VolumeApi extends VolumeController with ApiController {
  def get(i : models.Volume.Id) = Action(i).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  private final val queryOpts : JsonOptions.Options = Map("access" -> Seq("ADMIN"), "providers" -> Nil)
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

  def accessDelete(volumeId : models.Volume.Id, partyId : models.Party.Id) = Action(volumeId, Permission.ADMIN).async { implicit request =>
    (if (!(partyId === request.identity.id))
      VolumeAccess.delete(request.obj, partyId)
    else macros.Async(false)).map { _ =>
      result(request.obj)
    }
  }
}
