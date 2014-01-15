package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import org.mindrot.jbcrypt.BCrypt
import macros._
import dbrary._
import site._
import models._

private[controllers] sealed abstract class PartyController extends ObjectController[SiteParty] {
  /** ActionBuilder for party-targeted actions.
    * @param i target party id, defaulting to current user (site.identity)
    * @param p permission needed, or None if delegation is not allowed (must be self)
    */
  private[controllers] def action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    optionZip(i, p).fold[ActionFunction[SiteRequest.Base,Request]] {
      SiteAction.Auth ~> new ActionRefiner[SiteRequest.Auth,Request] {
        protected def refine[A](request : SiteRequest.Auth[A]) =
          if (i.fold(true)(_ === request.identity.id))
            request.identity.perSite(request).map { p =>
              Right(request.withObj(p))
            }
          else
            macros.Async(Left(Forbidden))
      }
    } { case (i, p) =>
      RequestObject.check[SiteParty](models.SiteParty.get(i)(_), p)
    }

  private[controllers] def Action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    SiteAction ~> action(i, p)

  protected val passwordInputMapping : Mapping[Option[String]]
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

  type AccountMapping = (String, Option[String], Option[String], Option[String])
  type PartyMapping = (Option[String], Option[Option[Orcid]], Option[String], Option[Option[DUNS]], Option[AccountMapping])
  type PartyForm = Form[PartyMapping]
  protected def partyForm(acct : Boolean) : PartyForm = Form(tuple(
    "name" -> OptionMapping(nonEmptyText),
    "orcid" -> OptionMapping(optional(of[Orcid])),
    "affiliation" -> OptionMapping(text),
    "duns" -> OptionMapping(optional(of[DUNS])),
    "" -> MaybeMapping(if (acct) Some(tuple(
      "auth" -> text,
      "email" -> OptionMapping(email),
      "password" -> passwordMapping,
      "openid" -> OptionMapping(text(0,256))
    )) else None)
  ))
  protected def formFill(implicit request : Request[_]) : PartyForm = {
    val e = request.obj.party
    val acct = adminAccount
    partyForm(acct.isDefined)
      .fill((Some(e.name), Some(e.orcid), Some(e.affiliation.getOrElse("")), Some(e.duns), acct.map(a => ("", Some(a.email), None, a.openid))))
  }

  def formForAccount(form : PartyForm)(implicit request : Request[_]) =
    form.value.fold[Option[Any]](adminAccount)(_._3).isDefined

  def update(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    def bad(form : PartyForm) =
      ABadForm[PartyMapping](views.html.party.edit(_), form)
    val form = formFill.bindFromRequest
    val party = request.obj.party
    val acct = adminAccount
    form.fold(bad _, {
      case (_, _, _, _, Some((cur, _, _, _))) if !acct.fold(false)(a => BCrypt.checkpw(cur, a.password)) =>
        bad(form.withError("cur_password", "password.incorrect"))
      case (name, orcid, affiliation, duns, accts) =>
        for {
          _ <- party.change(
	    name = name,
	    orcid = orcid,
	    affiliation = affiliation.map(Maybe(_).opt),
	    duns = duns.filter(_ => request.access == Permission.ADMIN))
          _ <- macros.Async.foreach[AccountMapping, Unit](accts, { case (_, email, password, openid) =>
            val a = acct.get
            a.change(
              email = email,
              password = password,
              openid = openid.map(Maybe(_).opt)
            )
          })
        } yield (result(request.obj))
    })
  }

  private[this] def create(error : PartyForm => BadFormException[PartyMapping], acct : Boolean) = SiteAction.access(Permission.ADMIN).async { implicit request =>
    def Error(form : PartyForm) =
      throw error(form)
    val form = partyForm(acct).bindFromRequest
    form.fold(Error _, {
      case (name, orcid, affiliation, duns, accts) =>
	for {
	  p <- Party.create(
	    name = name getOrElse Error(form.withError("name", "error.required")),
	    orcid = orcid.flatten,
	    affiliation = affiliation,
	    duns = duns.flatten)
          a <- macros.Async.map[AccountMapping, Account](accts, { case (_, email, password, openid) =>
	    Account.create(p,
	      email = email getOrElse Error(form.withError("email", "error.required")),
	      password = password,
	      openid)
          })
	  s <- p.perSite
	} yield (result(s))
    })
  }

  type AuthorizeMapping = (Permission.Value, Permission.Value, Boolean, Option[Date])
  type AuthorizeForm = Form[AuthorizeMapping]
  protected val authorizeForm : AuthorizeForm = Form(
    tuple(
      "access" -> Field.enum(Permission),
      "delegate" -> Field.enum(Permission),
      "pending" -> boolean,
      "expires" -> optional(jodaLocalDate)
    )
  )

  protected def authorizeFormFill(auth : Authorize, apply : Boolean = false) : AuthorizeForm =
    authorizeForm.fill((auth.access, auth.delegate, auth.authorized.isEmpty, auth.expires.map(_.toLocalDate)))

  protected final val maxExpiration = org.joda.time.Years.years(1)

  def authorizeChange(id : models.Party.Id, childId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(childId).flatMap(_.fold(ANotFound) { child =>
    def bad(form : AuthorizeForm) =
      AbadForm[AuthorizeMapping](f => PartyHtml.viewAdmin(authorizeChangeForm = Some((child, f))), form)
    val form = authorizeForm.bindFromRequest
    form.fold(bad, {
      case (access, delegate, pending, expires) =>
        val (exp, expok) = if (request.superuser) (expires, true)
          else {
            val maxexp = (new Date).plus(maxExpiration)
            val exp = expires.getOrElse(maxexp)
            (Some(exp), exp.isAfter(maxexp))
          }
        if (!expok)
          bad(form.withError("expires", "error.max", maxExpiration))
        else
          Authorize.set(childId, id, access, delegate, if (pending) None else Some(new Timestamp), exp.map(_.toLocalDateTime(org.joda.time.LocalTime.MIDNIGHT))).map { _ =>
            result(request.obj)
          }
      }
    )
    })
  }

  def authorizeDelete(id : models.Party.Id, other : models.Party.Id) = AdminAction(id).async { implicit request =>
    for {
      /* users can remove themselves from any relationship */
      _ <- models.Authorize.delete(id, other)
      _ <- models.Authorize.delete(other, id)
    } yield (result(request.obj))
  }

  def authorizeApply(id : models.Party.Id, parentId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(parentId).flatMap(_.fold(ANotFound) { parent =>
    authorizeForm.bindFromRequest.fold(
      AbadForm[AuthorizeMapping](f => PartyHtml.viewAdmin(authorizeWhich = Some(true), authorizeResults = Seq((parent, f))), _),
      { case (access, delegate, _, expires) =>
        Authorize.set(id, parentId, access, delegate, None, expires.map(_.toLocalDateTime(org.joda.time.LocalTime.MIDNIGHT))).map { _ =>
          result(request.obj)
        }
      }
    )})
  }
}

private[controllers] object PartyController extends PartyController {
  protected val passwordInputMapping = optional(text(7))
}

object PartyHtml extends PartyController {
  protected val passwordInputMapping =
    tuple(
      "once" -> optional(text(7)),
      "again" -> text
    ).verifying(Messages("password.again"), pa => pa._1.fold(true)(_ == pa._2))
    .transform[Option[String]](_._1, p => (p, p.getOrElse("")))

  def view(i : models.Party.Id) = Action(Some(i), Some(Permission.NONE)).async { implicit request =>
    for {
      parents <- request.obj.authorizeParents()
      children <- request.obj.authorizeChildren()
      vols <- request.obj.volumeAccess
      comments <- request.obj.party.account.fold[Future[Seq[Comment]]](macros.Async(Nil))(_.comments)
    } yield (Ok(views.html.party.view(parents, children, vols, comments)))
  }

  private val authorizeSearchForm = Form(
    "name" -> nonEmptyText
  )

  private[controllers] def viewAdmin(
    authorizeChangeForm : Option[(models.Party,AuthorizeForm)] = None,
    authorizeWhich : Option[Boolean] = None,
    authorizeSearchForm : Form[String] = authorizeSearchForm,
    authorizeResults : Seq[(models.Party,AuthorizeForm)] = Seq())(
    implicit request : Request[_]) = {
    val authorizeChange = authorizeChangeForm.map(_._1.id)
    for {
      children <- request.obj.authorizeChildren(true)
      parents <- request.obj.authorizeParents(true)
      authorizeForms = children
        .filter(t => authorizeChange.fold(true)(_ === t.childId))
        .map(t => (t.child, authorizeFormFill(t))) ++
        authorizeChangeForm
    } yield (views.html.party.authorize(parents, authorizeForms, authorizeWhich, authorizeSearchForm, authorizeResults))
  }
  
  def edit(i : models.Party.Id) = AdminAction(i) { implicit request =>
    Ok(views.html.party.edit(formFill))
  }

  def admin(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    viewAdmin().map(Ok(_))
  }

  def authorizeSearch(id : models.Party.Id, apply : Boolean) = AdminAction(id).async { implicit request =>
    val form = authorizeSearchForm.bindFromRequest
    form.fold(
      AbadForm[String](f => viewAdmin(authorizeWhich = Some(apply), authorizeSearchForm = f), _),
      name =>
        models.Party.searchForAuthorize(name, request.obj.party).flatMap { res =>
        viewAdmin(authorizeWhich = Some(apply), authorizeSearchForm = form, 
          authorizeResults = res.map(e => (e, authorizeForm.fill(
            if (apply) (Permission.NONE, Permission.NONE, true, None)
            else (Permission.NONE, Permission.NONE, false, Some((new Date).plus(maxExpiration)))))))
          .map(Ok(_))
        }
    )
  }

  def avatar(i : models.Party.Id, size : Int = 64) = Action(Some(i), Some(Permission.NONE)).async { implicit request =>
    request.obj.avatar.flatMap(_.fold(
      macros.Async(Found("http://gravatar.com/avatar/"+request.obj.party.account.fold("none")(a => store.MD5.hex(a.email.toLowerCase))+"?s="+size+"&d=mm")))(
      AssetController.assetResult(_)))
  }

  type AvatarMapping = Unit
  type AvatarForm = Form[AvatarMapping]
  val avatarForm = Form("file" -> ignored(()))

  def uploadAvatar(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    def Error(form : AvatarForm) =
      throw new BadFormException[AvatarMapping](views.html.party.edit(formFill, _))(form)
    val form = avatarForm.bindFromRequest
    form.fold(Error _, { _ =>
      val file = request.body.asMultipartFormData.flatMap(_.file("file")) getOrElse
	Error(form.withError("file", "error.required"))
      val fmt = AssetFormat.getFilePart(file).filter(_.isImage) getOrElse
	Error(form.withError("file", "file.format.unknown", file.contentType.getOrElse("unknown")))
      request.obj.setAvatar(file.ref, fmt, Maybe(file.filename).opt).map(_ =>
	result(request.obj))
    })
  }
}

object PartyApi extends PartyController {
  protected val passwordInputMapping = OptionMapping(text(7))

  def get(partyId : models.Party.Id) = Action(Some(partyId), Some(Permission.NONE)).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  def authorizeGet(partyId : models.Party.Id) = AdminAction(partyId).async { implicit request =>
    for {
      parents <- request.obj.authorizeParents(true)
      children <- request.obj.authorizeChildren(true)
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
