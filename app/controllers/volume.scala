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
    SiteAction andThen action(i, p)

  protected def searchResults(implicit request : SiteRequest[AnyContent]) : (VolumeController.SearchForm, Future[Seq[Volume]]) = {
    val form = new VolumeController.SearchForm()._bind
    (form, Volume.search(form.query.get, form.party.get))
  }

  def update(i : models.Volume.Id) = Action(i, Permission.EDIT).async { implicit request =>
    val vol = request.obj
    val form = new VolumeController.EditForm(None)._bind
    for {
      cite <- form.getCitation
      _ <- vol.change(name = form.name.get orElse cite.flatMap(_.flatMap(_.title)),
        alias = form.alias.get.map(Maybe(_).opt),
        body = form.body.get)
      _ <- cite.foreachAsync(vol.setCitation)
    } yield (result(vol))
  }

  def create(owner : Option[Party.Id]) = ContributeAction(owner).async { implicit request =>
    val form = new VolumeController.CreateForm()._bind
    for {
      cite <- form.getCitation
      vol <- models.Volume.create(form.name.get orElse cite.flatMap(_.flatMap(_.title)) getOrElse "New Volume",
        alias = form.alias.get.flatMap(Maybe(_).opt),
        body = form.body.get.flatten)
      _ <- VolumeAccess.set(vol, owner.getOrElse(request.identity.id), Permission.ADMIN, Permission.CONTRIBUTE)
      _ <- cite.foreachAsync(vol.setCitation)
    } yield (result(vol))
  }

  protected def ContributeAction(e : Option[models.Party.Id]) =
    PartyController.Action(e, Some(Permission.CONTRIBUTE)) andThen
      new ActionFilter[PartyController.Request] {
        protected def filter[A](request : PartyController.Request[A]) =
          request.obj.party.access.map(a => if (a.site < Permission.PUBLIC) Some(Forbidden) else None)
      }

  protected def accessForm(access : VolumeAccess)(implicit request : Request[_]) =
    new VolumeController.AccessForm(access.party)._fill(access)

  def accessChange(id : models.Volume.Id, e : models.Party.Id) = Action(id, Permission.ADMIN).async { implicit request =>
    for {
      who <- models.Party.get(e).map(_.getOrElse(throw NotFoundException))
      via <- request.obj.adminAccessVia
      form = new VolumeController.AccessForm(who, via.exists(_ === who))._bind
      _ <- if (form.delete.get)
          VolumeAccess.set(request.obj, e)
        else
          (if (!request.superuser && form.isRestricted)
            via.mapAsync(_.party.access.map(_.site))
          else async(Seq(Permission.ADMIN))).flatMap { viaa =>
            if (!viaa.exists(_ >= Permission.CONTRIBUTE))
              form.withGlobalError("access.grant.restricted", form.party.name)._throw
            else
              VolumeAccess.set(request.obj, e, individual = max(form.individual.get, form.children.get), children = form.children.get)
          }
    } yield (result(request.obj))
  }

  def accessSearch(id : models.Volume.Id) =
    Action(id, Permission.ADMIN).async { implicit request =>
      val form = new VolumeController.AccessSearchForm()._bind
      for {
        res <- models.Party.search(Some(form.name.get), volume = Some(request.obj))
        r <- if (request.isApi) macros.async(Ok(JsonArray.map[Party, JsonRecord](_.json)(res)))
          else VolumeHtml.viewAdmin(accessSearchForm = Some(form), accessResults = res)
            .map(Ok(_))
      } yield (r)
    }
}

object VolumeController extends VolumeController {
  final class SearchForm(implicit request : SiteRequest[_])
    extends HtmlForm[SearchForm](
      routes.VolumeHtml.search,
      views.html.volume.search(Nil, _))
    with NoCsrfForm {
    val query = Field(Mappings.maybeText)
    val party = Field(OptionMapping(Forms.of[Party.Id]))
  }

  private val citationMapping = Forms.tuple(
    "head" -> Mappings.maybeText,
    "url" -> Forms.optional(Forms.of[URL]),
    "authors" -> Forms.seq(Forms.nonEmptyText),
    "year" -> Forms.optional(Forms.number(1900, 2900))
  ).verifying("citation.invalid", _ match {
    case (None, None, Nil, None) => true
    case (Some(_), _, _, _) => true
    case (None, Some(url), _, _) if url.getProtocol.equals("hdl") || url.getProtocol.equals("doi") => true
    case _ => false
  }).transform[Option[Citation]]({
    case (None, None, Nil, None) => None
    case (head, url, authors, year) => Some(Citation(head = head.getOrElse(""), url = url, authors = if (authors.nonEmpty) Some(authors.toIndexedSeq) else None, year = year.map(_.toShort)))
  }, {
    case None => (None, None, Nil, None)
    case Some(cite) => (Some(cite.head), cite.url, cite.authors.fold[Seq[String]](Nil)(_.toSeq), cite.year.map(_.toInt))
  })

  trait VolumeForm extends FormView {
    def actionName : String
    def formName : String = actionName + " Volume"

    val name = Field(OptionMapping(Mappings.nonEmptyText))
    val alias = Field(OptionMapping(Forms.text(maxLength = 64)))
    val body = Field(OptionMapping(Mappings.maybeText))
    val citation = Field(OptionMapping(citationMapping))
    def getCitation : Future[Option[Option[Citation]]] =
      citation.get.mapAsync(_.mapAsync(_.copy(title = name.get).lookup(false)))
  }

  final class EditForm(cite : Option[Citation])(implicit request : Request[_])
    extends HtmlForm[EditForm](
      routes.VolumeHtml.update(request.obj.id),
      views.html.volume.edit(_)) with VolumeForm {
    def actionName = "Update"
    override def formName = "Edit Volume"
    name.fill(Some(request.obj.name))
    alias.fill(Some(request.obj.alias.getOrElse("")))
    body.fill(Some(request.obj.body))
    citation.fill(Some(cite))
  }

  final class CreateForm(implicit request : PartyController.Request[_])
    extends HtmlForm[CreateForm](
      routes.VolumeHtml.create(Some(request.obj.party.id)),
      views.html.volume.edit(_)) with VolumeForm {
    def actionName = "Create"
  }

  final class AccessForm(val party : Party, own : Boolean = false)(implicit request : Request[_])
    extends AHtmlForm[AccessForm](
      routes.VolumeHtml.accessChange(request.obj.id, party.id),
      f => VolumeHtml.viewAdmin(accessChangeForm = Some(f))) {
    def partyId = party.id
    /** Does the affected party corresponding to a restricted-access group? */
    def isGroup = party._id <= 0
    val individual = Field(Mappings.enum(Permission,
      maxId = if (isGroup) Some(Permission.SHARED.id) else None,
      minId = (if (own) Permission.ADMIN else Permission.NONE).id))
    val children = Field(Mappings.enum(Permission,
      maxId = if (isGroup) Some(Permission.SHARED.id) else None))
    val delete = Field(if (own) Forms.boolean.verifying("access.delete.self", !_) else Forms.boolean).fill(false)
    private[controllers] def _fill(a : VolumeAccess) : this.type = {
      assert(a.party === party)
      individual.fill(a.individual)
      children.fill(a.children)
      this
    }
    /** Does granting this access level require CONTRIBUTE-level authorization? */
    def isRestricted : Boolean =
      isGroup && children.get > Permission.NONE
  }

  final class AccessSearchForm(implicit request : Request[_])
    extends AHtmlForm[AccessSearchForm](
      routes.VolumeHtml.accessSearch(request.obj.id),
      f => VolumeHtml.viewAdmin(accessSearchForm = Some(f))) {
    val name = Field(Mappings.nonEmptyText)
  }

  def zip(i : Volume.Id) = Action(i).async { implicit request =>
    request.obj.auditDownload.map { _ =>
      AssetController.zipResult(store.Zip.volume(request.obj), "databrary-" + request.obj.id)
    }
  }

  def thumb(v : models.Volume.Id, size : Int = AssetController.defaultThumbSize) =
    Action(v).async { implicit request =>
      request.obj.thumb.flatMap(_.fold(
        async(Found("/public/images/draft.png")))(
        a => SlotAssetController.getFrame(Left(0.25f), size)(request.withObj(a))))
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
      records <- vol.records()
      excerpts <- vol.excerpts
      citation <- vol.citation
      funding <- vol.funding
      comments <- vol.comments
      tags <- vol.tags
    } yield (Ok(views.html.volume.view(summary, access, top, sessions.map(_._1), records, excerpts, citation, funding, comments, tags)))
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
    request.obj.citation
    .map(new VolumeController.EditForm(_))
    .flatMap(_.Ok)
  }

  def add(e : Option[models.Party.Id]) = ContributeAction(e).async { implicit request =>
    (new CreateForm).Ok
  }

  /* just exists in angular */
  def spreadsheet(i : models.Volume.Id) = view(i)

  private[controllers] def viewAdmin(
    accessChangeForm : Option[AccessForm] = None,
    accessSearchForm : Option[AccessSearchForm] = None,
    accessResults : Seq[Party] = Nil)(
    implicit request : Request[_]) = {
    val change = accessChangeForm.map(_.partyId).toSet
    for {
      access <- request.obj.partyAccess()
      forms = access
        .filterNot(a => change.contains(a.partyId))
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

  private final val queryOpts : JsonOptions.Options = Map("access" -> Seq("ADMIN"), "citation" -> Nil)
  def query = SiteAction.async { implicit request =>
    for {
      vl <- searchResults._2
      vols <- vl.mapAsync[JsonRecord, Seq[JsonRecord]](_.json(queryOpts))
    } yield (Ok(JsonArray(vols)))
  }

  def accessGet(volumeId : Volume.Id) = Action(volumeId, Permission.ADMIN).async { implicit request =>
    for {
      parents <- request.obj.partyAccess()
    } yield (Ok(JsonRecord.map[VolumeAccess](a =>
      JsonRecord(a.partyId) ++ (a.json - "volume"))(parents)))
  }

  def accessDelete(volumeId : Volume.Id, partyId : Party.Id) =
    Action(volumeId, Permission.ADMIN).async { implicit request =>
      (if (!(partyId === request.identity.id))
        VolumeAccess.set(request.obj, partyId)
      else macros.async(false)).map { _ =>
        result(request.obj)
      }
    }

  final class FundingForm(funderId : Funder.Id)(implicit request : Request[_])
    extends ApiForm(routes.VolumeApi.fundingChange(request.obj.id, funderId)) {
    val awards = Field(Forms.seq(Mappings.nonEmptyText))
  }

  def funderSearch(query : String, all : Boolean = false) =
    SiteAction.async { implicit request =>
      Funder.search(query, all).map(r => Ok(JsonArray.map[Funder,JsonObject](_.json)(r)))
    }

  def fundingChange(volumeId : Volume.Id, funderId : Funder.Id) =
    Action(volumeId, Permission.EDIT).async { implicit request =>
      val form = new FundingForm(funderId)._bind
      VolumeFunding.set(request.obj, funderId, Some(form.awards.get.toIndexedSeq)).map { r =>
        if (r) result(request.obj)
        else form.withGlobalError("funder.notfound", funderId)._throw
      }
    }

  def fundingDelete(volumeId : Volume.Id, funderId : Funder.Id) =
    Action(volumeId, Permission.EDIT).async { implicit request =>
      VolumeFunding.set(request.obj, funderId, None).map { _ =>
        result(request.obj)
      }
    }
}
