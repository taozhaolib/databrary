package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import org.mindrot.jbcrypt.BCrypt
import macros._
import dbrary._
import site._
import models._

sealed abstract class PartyController extends ObjectController[SiteParty] {
  /** ActionBuilder for party-targeted actions.
    * @param i target party id, defaulting to current user (site.identity)
    * @param p permission needed, or None if delegation is not allowed (must be self)
    */
  private[controllers] def action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    zip[Party.Id, Permission.Value, ActionFunction[SiteRequest.Base,Request]](i, p, (i, p) =>
      RequestObject.check[SiteParty](models.SiteParty.get(i)(_), p))
    .getOrElse {
      SiteAction.Auth ~> new ActionRefiner[SiteRequest.Auth,Request] {
        protected def refine[A](request : SiteRequest.Auth[A]) =
          if (i.forall(_ === request.identity.id))
            request.identity.perSite(request).map { p =>
              Right(request.withObj(p))
            }
          else
            macros.Async(Left(Forbidden))
      }
    }

  private[controllers] def Action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    SiteAction ~> action(i, p)

  protected def passwordInputMapping : Mapping[Option[String]] = Forms.optional(Forms.text(7))
  type PasswordMapping = Mapping[Option[String]]
  def passwordMapping : PasswordMapping = 
    passwordInputMapping
    .transform[Option[String]](
      _.map(BCrypt.hashpw(_, BCrypt.gensalt)),
      _.map(_ => "")
    )

  protected def AdminAction(i : models.Party.Id, delegate : Boolean = true) =
    Action(Some(i), if (delegate) Some(Permission.ADMIN) else None)

  protected def adminAccount(implicit request : Request[_]) : Option[Account] =
    request.obj.party.account.filter(_ === request.identity || request.superuser)

  abstract sealed class PartyForm(action : Call)(implicit request : SiteRequest[_])
    extends HtmlForm[PartyForm](action,
      views.html.party.edit(_)) {
    def actionName : String
    def formName : String = actionName + " Party"
    val name : Field[Option[String]]
    val orcid = Field(OptionMapping(Forms.optional(Forms.of[Orcid])))
    val affiliation = Field(OptionMapping(Forms.text))
    val duns = Field(OptionMapping(Forms.optional(Forms.of[DUNS])))
  }
  sealed trait AccountForm extends PartyForm {
    override def formName : String = actionName + " Account"
    val email : Field[Option[String]]
    val password = Field(passwordMapping).fill(None)
    val openid = Field(OptionMapping(Forms.text(0,256)))
  }

  abstract sealed class EditForm(implicit request : Request[_])
    extends PartyForm(routes.PartyHtml.update(request.obj.id)) {
    def actionName = "Update"
    override def formName = "Edit Profile"
    def party = request.obj.party
    def accountForm : Option[(Account, AccountEditForm)]
    val name = Field(OptionMapping(Forms.nonEmptyText)).fill(Some(party.name))
    val avatar = OptionalFile()
    orcid.fill(Some(party.orcid))
    affiliation.fill(Some(party.affiliation.getOrElse("")))
    duns.fill(Some(party.duns))
  }
  final class PartyEditForm(implicit request : Request[_]) extends EditForm {
    def accountForm = None
  }
  final class AccountEditForm(account : Account)(implicit request : Request[_]) extends EditForm with AccountForm {
    val auth = Field(Forms.text.verifying("password.incorrect",
      s => s.isEmpty || BCrypt.checkpw(s, account.password))).fill("")
    def accountForm = if (auth.get.isEmpty) None else Some((account, this))
    val email = Field(OptionMapping(Forms.email)).fill(Some(account.email))
    openid.fill(account.openid)
  }
  protected def editForm(implicit request : Request[_]) : EditForm =
    adminAccount.fold[EditForm](new PartyEditForm)(new AccountEditForm(_))

  abstract sealed class CreateForm(implicit request : SiteRequest[_])
    extends PartyForm(routes.PartyHtml.create()) {
    def actionName = "Create"
    val name = Field(Mappings.some(Forms.nonEmptyText))
  }
  final class PartyCreateForm(implicit request : SiteRequest[_]) extends CreateForm
  final class AccountCreateForm(implicit request : SiteRequest[_]) extends CreateForm with AccountForm {
    val email = Field(Mappings.some(Forms.email))
  }
  protected def createForm(acct : Boolean)(implicit request : SiteRequest[_]) : CreateForm =
    if (acct) new AccountCreateForm else new PartyCreateForm

  def update(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    val form = editForm._bind
    val party = request.obj.party
    for {
      _ <- party.change(
	name = form.name.get,
	orcid = form.orcid.get,
	affiliation = form.affiliation.get.map(Maybe(_).opt),
	duns = form.duns.get.filter(_ => request.access.direct == Permission.ADMIN)
      )
      _ <- macros.Async.foreach[(Account, AccountEditForm), Unit](form.accountForm, { case (account, form) =>
	account.change(
	  email = form.email.get,
	  password = form.password.get,
	  openid = form.openid.get.map(Maybe(_).opt)
	)
      })
      _ <- macros.Async.foreach(form.avatar.get, { file : form.FilePart =>
	val fmt = AssetFormat.getFilePart(file).filter(_.isImage) getOrElse
	  form.avatar.withError("file.format.unknown", file.contentType.getOrElse("unknown"))._throw
	request.obj.setAvatar(file.ref, fmt, Maybe(file.filename).opt)
      })
    } yield (result(request.obj))
  }

  def create(acct : Boolean = false) : Action[_] = SiteAction.rootAccess().async { implicit request =>
    val form = createForm(acct)._bind
    for {
      p <- Party.create(
	name = form.name.get.get,
	orcid = form.orcid.get.flatten,
	affiliation = form.affiliation.get,
	duns = form.duns.get.flatten)
      a <- macros.Async.map[AccountCreateForm, Account](cast[AccountCreateForm](form), form =>
	Account.create(p,
	  email = form.email.get.get,
	  password = form.password.get,
	  openid = form.openid.get)
      )
      s <- p.perSite
    } yield (result(s))
  }

  def authorizeChange(id : models.Party.Id, childId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(childId).flatMap(_.fold(ANotFound) { child =>
    val form = new PartyController.AuthorizeChildForm(child)._bind
    (if (form.delete.get)
      models.Authorize.delete(childId, id)
    else
      Authorize.set(childId, id,
	max(form.inherit.get, form.permission.get),
	max(form.direct.get, form.permission.get),
	if (form.pending.get) None else Some(new Timestamp),
	form.expires.get.map(_.toLocalDateTime(org.joda.time.LocalTime.MIDNIGHT)))
      .flatMap { _ =>
	Authorize.Info.set(childId, id, form.info.get)
      }
    ).map(_ => result(request.obj))
    })
  }

  def authorizeDelete(id : models.Party.Id, other : models.Party.Id) = AdminAction(id).async { implicit request =>
    for {
      /* users can remove themselves from any relationship */
      _ <- models.Authorize.delete(id, other)
      _ <- models.Authorize.delete(other, id)
    } yield (result(request.obj))
  }

  private def delegates(party : Party) : Future[Seq[Account]] =
    party.authorizeChildren().map(_.filter(_.direct >= Permission.ADMIN).flatMap(_.child.account)
      ++ party.account)

  def authorizeApply(id : models.Party.Id, parentId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(parentId).flatMap(_.fold(ANotFound) { parent =>
    val form = new PartyController.AuthorizeApplyForm(parent)._bind
    for {
      dl <- delegates(parent)
      _ <- Authorize.set(id, parentId, form.inherit.get, Permission.NONE, None, None)
      _ <- Authorize.Info.set(id, parentId, form.info.get)
      _ <- Mail.send(
	to = (dl.map(_.email) :+ Messages("mail.authorize")).mkString(", "),
	subject = Messages("mail.authorize.subject"),
	body = Messages("mail.authorize.body", routes.PartyHtml.admin(parentId).absoluteURL(true),
	  request.obj.party.name + request.identity.email.fold("")(" <" + _ + ">"),
	  parent.name)
      ).recover {
	case ServiceUnavailableException => ()
      }
    } yield (result(request.obj))
    })
  }
}

object PartyController extends PartyController {
  private final val maxExpiration = org.joda.time.Years.years(2)

  sealed trait AuthorizeBaseForm extends StructForm {
    val inherit = Field(Forms.default(Mappings.enum(Permission), Permission.NONE))
    val info = Field(Forms.optional(Forms.nonEmptyText)).fill(None)
    def copyFrom(f : AuthorizeForm) : this.type = {
      inherit.fill(f.inherit.get)
      info.fill(f.info.get)
      this
    }
  }
  sealed trait AuthorizeFullForm extends AuthorizeBaseForm {
    val direct = Field(Forms.default(Mappings.enum(Permission), Permission.NONE))
    val pending = Field(Forms.boolean)
    val delete = Field(Forms.boolean).fill(false)
    val expires = Field(Forms.optional(Forms.jodaLocalDate))
    private[controllers] def _fill(auth : Authorize) : this.type = {
      inherit.fill(auth.inherit)
      direct.fill(auth.direct)
      pending.fill(auth.authorized.isEmpty)
      expires.fill(auth.expires.map(_.toLocalDate))
      info.fill(auth.info)
      this
    }
  }
  sealed abstract class AuthorizeForm(action : Call)(implicit request : Request[_])
    extends AHtmlForm[AuthorizeForm](action,
      f => PartyHtml.viewAdmin(Seq(f)))
    with AuthorizeBaseForm {
    def _apply : Boolean
  }
  sealed trait AuthorizeOtherForm extends AuthorizeForm {
    def targetParty : Party
  }
  final class AuthorizeChildForm(val child : Party)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeChange(request.obj.id, child.id))
    with AuthorizeFullForm
    with AuthorizeOtherForm {
    def targetParty = child
    def _apply = false
    val permission = Field(Forms.default(Mappings.enum(Permission), Permission.NONE))
    private[this] val maxexp = (new Date).plus(maxExpiration)
    override val expires = Field(if (request.superuser) Forms.optional(Forms.jodaLocalDate)
      else Mappings.some(Forms.jodaLocalDate, maxexp)
	.verifying(validation.Constraint[Option[Date]]("constraint.max", maxExpiration) { d =>
	  if (d.forall(_.isAfter(maxexp))) validation.Invalid(validation.ValidationError("error.max", maxExpiration))
	  else validation.Valid
	}))
    private[controllers] override def _fill(auth : Authorize) : this.type = {
      assert(request.obj === auth.parent)
      assert(child === auth.child)
      permission.fill(auth.permission)
      super._fill(auth)
    }
  }
  final class AuthorizeApplyForm(val parent : Party)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeApply(request.obj.id, parent.id))
    with AuthorizeOtherForm {
    def targetParty = parent
    def _apply = true
  }
  final class AuthorizeSearchForm(val _apply : Boolean)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeSearch(request.obj.id, _apply)) {
    val name = Field(Forms.nonEmptyText)
    val notfound = Field(Forms.boolean).fill(false)
  }
  final class AuthorizeAdminForm(val authorize : Authorize)(implicit request : SiteRequest[_])
    extends StructForm(routes.PartyHtml.authorizeChange(authorize.parentId, authorize.childId))
    with AuthorizeFullForm {
    _fill(authorize)
  }
}

object PartyHtml extends PartyController {
  import PartyController._

  override protected val passwordInputMapping =
    Forms.tuple(
      "once" -> super.passwordInputMapping,
      "again" -> Forms.text
    ).verifying(Messages("password.again"), pa => pa._1.forall(_ == pa._2))
    .transform[Option[String]](_._1, p => (p, p.getOrElse("")))

  def view(i : models.Party.Id) = Action(Some(i), Some(Permission.NONE)).async { implicit request =>
    for {
      parents <- request.obj.party.authorizeParents()
      children <- request.obj.party.authorizeChildren()
      vols <- request.obj.volumeAccess
      comments <- request.obj.party.account.fold[Future[Seq[Comment]]](macros.Async(Nil))(_.comments)
    } yield (Ok(views.html.party.view(parents, children, vols, comments)))
  }

  private[controllers] def viewAdmin(
    authorizeForms : Seq[AuthorizeForm] = Nil)(
    implicit request : Request[_]) = {
    val change = authorizeForms.collect { case o : AuthorizeOtherForm => o.targetParty.id.unId }.toSet
    val search = Set(false, true) -- authorizeForms.collect { case a : AuthorizeSearchForm => a._apply }
    for {
      parents <- request.obj.party.authorizeParents(true)
      children <- request.obj.party.authorizeChildren(true)
      forms = children
        .filterNot(t => change.contains(t.childId.unId))
        .map(t => new AuthorizeChildForm(t.child)._fill(t)) ++
	search.map(new AuthorizeSearchForm(_)) ++
        authorizeForms
    } yield (views.html.party.authorize(parents, forms))
  }
  
  def edit(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    editForm.Ok
  }

  def createNew(acct : Boolean = false) = SiteAction.rootAccess().async { implicit request =>
    createForm(acct).Ok
  }

  def admin(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    viewAdmin().map(Ok(_))
  }

  def authorizeSearch(id : models.Party.Id, apply : Boolean) = AdminAction(id).async { implicit request =>
    val form = new AuthorizeSearchForm(apply)._bind
    if (form.notfound.get)
      for {
	_ <- Mail.send(
	  to = Messages("mail.authorize"),
	  subject = Messages("mail.authorize.subject"),
	  body = Messages("mail.authorize.body", routes.PartyHtml.view(id).absoluteURL(true),
	    request.obj.party.name + request.identity.email.fold("")(" <" + _ + ">") + request.obj.party.affiliation.fold("")(" (" + _ + ")"),
	    form.name.get + form.info.get.fold("")(" (" + _ + ")")))
      } yield (Ok("request sent"))
    else
      for {
        res <- models.Party.searchForAuthorize(form.name.get, request.obj.party)
        r <- viewAdmin(form +: res.map(e =>
	    (if (apply) new AuthorizeApplyForm(e) else new AuthorizeChildForm(e)).copyFrom(form)))
      } yield (Ok(r))
  }

  def authorizeAdmin = SiteAction.rootAccess().async { implicit request =>
    Authorize.getAll.map { all =>
      val (pend, rest) = all.span(_.authorized.isEmpty)
      val (exp, act) = rest.span(!_.valid)
      Ok(views.html.party.authorizeAdmin(pend.map(new AuthorizeAdminForm(_)), act, exp))
    }
  }

  def avatar(i : models.Party.Id, size : Int = 64) = Action(Some(i), Some(Permission.NONE)).async { implicit request =>
    request.obj.avatar.flatMap(_.fold(
      macros.Async(Found("http://gravatar.com/avatar/"+request.obj.party.account.fold("none")(a => store.MD5.hex(a.email.toLowerCase))+"?s="+size+"&d=mm")))(
      AssetController.assetResult(_)))
  }

}

object PartyApi extends PartyController {
  override protected val passwordInputMapping = OptionMapping(Forms.text(7))

  def get(partyId : models.Party.Id) = Action(Some(partyId), Some(Permission.NONE)).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  def authorizeGet(partyId : models.Party.Id) = AdminAction(partyId).async { implicit request =>
    for {
      parents <- request.obj.party.authorizeParents(true)
      children <- request.obj.party.authorizeChildren(true)
    } yield (Ok(JsonObject(
      'parents -> JsonRecord.map[Authorize](a => JsonRecord(a.parentId,
        'party -> a.parent.json) ++
        a.json)(parents),
      'children -> JsonRecord.map[Authorize](a => JsonRecord(a.childId,
        'party -> a.child.json) ++
        a.json)(children)
      ).obj))
  }

  def authorizeSearch(partyId : models.Party.Id, name : String) = AdminAction(partyId).async { implicit request =>
    models.Party.searchForAuthorize(name, request.obj.party)
      .map(r => Ok(JsonRecord.map[Party](_.json)(r)))
  }
}
