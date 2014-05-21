package controllers

import scala.concurrent.Future
import play.api._
import          mvc._
import          data._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import java.net.URL
import macros._
import macros.async._
import dbrary._
import site._
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

  private val citationMapping = Forms.tuple(
    "head" -> Mappings.maybeText,
    "url" -> Forms.optional(Forms.of[URL]),
    "body" -> Mappings.maybeText
  ).verifying("citation.invalid", _ match {
    case (None, None, None) => true
    case (Some(_), _, _) => true
    case (None, Some(url), _) if url.getProtocol.equals("hdl") || url.getProtocol.equals("doi") => true
    case _ => false
  }).transform[Option[Citation]]({
    case (None, None, None) => None
    case (head, url, body) => Some(Citation(head.getOrElse(""), url, body))
  }, {
    case None => (None, None, None)
    case Some(Citation(head, url, body, _)) => (Some(head), url, body)
  })
  private def citationFill(cite : Citation) : Future[Citation] =
    if (cite.head.isEmpty) cite.lookup else async(cite)
  private def citationSet(volume : Volume, form : VolumeController.VolumeForm) = {
    val cites = form.citation.get
    for {
      _ <- async.when(cites.nonEmpty,
	cites.flatten.mapAsync(citationFill)
	.flatMap(volume.setCitations _))
      _ <- form.study.get.foreachAsync(
	_.mapAsync(citationFill(_))
	.flatMap(volume.setStudyCitation _))
    } yield ()
  }

  protected def editFormFill(implicit request : Request[_]) =
    request.obj.citations.map(new VolumeController.EditForm(_))

  def update(i : models.Volume.Id) = Action(i, Permission.EDIT).async { implicit request =>
    val vol = request.obj
    for {
      form <- editFormFill
      _ = form._bind
      _ <- vol.change(name = form.name.get,
	alias = form.alias.get.map(Maybe(_).opt),
	body = form.body.get)
      _ <- citationSet(vol, form)
    } yield (result(vol))
  }

  def create(owner : Option[Party.Id]) = ContributeAction(owner).async { implicit request =>
    val form = new VolumeController.CreateForm()._bind
    for {
      vol <- models.Volume.create(form.name.get.get, form.alias.get.flatMap(Maybe(_).opt), form.body.get.flatten)
      _ <- citationSet(vol, form)
      _ <- VolumeAccess.set(vol, owner.getOrElse(request.identity.id), Permission.ADMIN, Permission.CONTRIBUTE)
    } yield (result(vol))
  }

  protected def ContributeAction(e : Option[models.Party.Id]) =
    PartyController.Action(e, Some(Permission.CONTRIBUTE)) ~>
      new ActionHandler[PartyController.Request] {
        protected def handle[A](request : PartyController.Request[A]) =
	  request.obj.party.access.map(a => if (a.group < Permission.VIEW) Some(Forbidden) else None)
      }

  protected def accessForm(access : VolumeAccess)(implicit request : Request[_]) =
    new VolumeController.AccessForm(access.party)._fill(access)

  def accessChange(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    for {
      who <- models.Party.get(e).map(_.getOrElse(throw NotFoundException))
      via <- request.obj.adminAccessVia
      form = new VolumeController.AccessForm(who, via.exists(_ === who))._bind
      viaa <- via.mapAsync(_.party.access.map(_.group))
      _ <- if (form.delete.get)
	  VolumeAccess.delete(request.obj, e)
	else
	  (if (form.isRestricted)
	    via.mapAsync(_.party.access.map(_.group))
	  else async(Seq(Permission.ADMIN))).flatMap { viaa =>
	    if (!viaa.exists(_ >= Permission.CONTRIBUTE))
	      form.withGlobalError("access.grant.restricted", form.party.name)._throw
	    else
	      VolumeAccess.set(request.obj, e, access = max(form.access.get, form.inherit.get), inherit = form.inherit.get, funding = form.funding.get)
	  }
    } yield (result(request.obj))
  }

  def accessSearch(id : models.Volume.Id) =
    Action(id, Permission.ADMIN).async { implicit request =>
      val form = new VolumeController.AccessSearchForm()._bind
      for {
	res <- models.Party.searchForVolumeAccess(form.name.get, request.obj)
	r <- if (request.isApi) macros.async(Ok(JsonRecord.map[Party](_.json)(res)))
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
    val query = Field(Mappings.maybeText)
    val party = Field(OptionMapping(Forms.of[Party.Id]))
  }

  trait VolumeForm extends FormView {
    def actionName : String
    def formName : String = actionName + " Volume"

    val name : Field[Option[String]]
    val alias = Field(OptionMapping(Forms.text(maxLength = 64)))
    val body = Field(OptionMapping(Mappings.maybeText))
    val study = Field(OptionMapping(citationMapping))
    val citation = Field(Forms.seq(citationMapping))
  }

  final class EditForm(cites : Seq[Citation])(implicit request : Request[_])
    extends HtmlForm[EditForm](
      routes.VolumeHtml.update(request.obj.id),
      views.html.volume.edit(_)) with VolumeForm {
    def actionName = "Update"
    override def formName = "Edit Volume"
    val name = Field(OptionMapping(Mappings.nonEmptyText)).fill(Some(request.obj.name))
    body.fill(Some(request.obj.body))
    alias.fill(Some(request.obj.alias.getOrElse("")))
    study.fill(Some(cites.headOption.filter(_.study)))
    citation.fill(cites.dropWhile(_.study).map(Some(_)) :+ None)
  }

  final class CreateForm(implicit request : PartyController.Request[_])
    extends HtmlForm[CreateForm](
      routes.VolumeHtml.create(Some(request.obj.party.id)),
      views.html.volume.edit(_)) with VolumeForm {
    def actionName = "Create"
    val name = Field(Mappings.some(Mappings.nonEmptyText))
  }

  final class AccessForm(val party : Party, own : Boolean = false)(implicit request : Request[_])
    extends AHtmlForm[AccessForm](
      routes.VolumeHtml.accessChange(request.obj.id, party.id),
      f => VolumeHtml.viewAdmin(accessChangeForm = Some(f))) {
    def partyId = party.id
    /** Does the affected party corresponding to a restricted-access group? */
    def isGroup = party.id.unId <= 0
    val access = Field(Mappings.enum(Permission,
      maxId = Some((if (isGroup) Permission.DOWNLOAD else Permission.ADMIN).id),
      minId = (if (own) Permission.ADMIN else Permission.NONE).id))
    val inherit = Field(Mappings.enum(Permission,
      maxId = Some(if (isGroup) Permission.DOWNLOAD.id else Permission.EDIT.id)))
    val funding = Field(Mappings.maybeText)
    val delete = Field(if (own) Forms.boolean.verifying("access.delete.self", !_) else Forms.boolean).fill(false)
    private[controllers] def _fill(a : VolumeAccess) : this.type = {
      assert(a.party === party)
      access.fill(a.access)
      inherit.fill(a.inherit)
      funding.fill(a.funding)
      this
    }
    /** Does granting this access level require CONTRIBUTE-level authorization? */
    def isRestricted : Boolean =
      isGroup && inherit.get > Permission.NONE
  }

  final class AccessSearchForm(implicit request : Request[_])
    extends AHtmlForm[AccessSearchForm](
      routes.VolumeHtml.accessSearch(request.obj.id),
      f => VolumeHtml.viewAdmin(accessSearchForm = Some(f))) {
    val name = Field(Mappings.nonEmptyText)
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

  def viewSearch(implicit request : SiteRequest[AnyContent]) = {
    val (form, res) = searchResults
    for {
      vl <- res
      vols <- vl.mapAsync(vol =>
	vol.partyAccess(Permission.ADMIN).map(a => (vol, a.map(_.party))))
    } yield (Ok(views.html.volume.search(vols, form)))
  }

  def search = SiteAction.async(viewSearch(_))

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
      vols <- vl.mapAsync[JsonRecord, Seq[JsonRecord]](_.json(queryOpts))
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
    else macros.async(false)).map { _ =>
      result(request.obj)
    }
  }
}
