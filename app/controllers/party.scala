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

  protected val RootAction =
    SiteAction ~> RequestObject.check[SiteParty](Party.Root.perSite(_).map(Some(_)), Permission.ADMIN)

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
    form.value.fold[Option[Any]](adminAccount)(_._5).isDefined

  def update(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    val party = request.obj.party
    def bad(form : PartyForm) =
      ABadForm[PartyMapping](views.html.party.edit(Some(party), _), form)
    val form = formFill.bindFromRequest
    val acct = adminAccount
    form.fold(bad _, {
      case (_, _, _, _, Some((cur, _, _, _))) if !acct.exists(a => BCrypt.checkpw(cur, a.password)) =>
        bad(form.withError("cur_password", "password.incorrect"))
      case (name, orcid, affiliation, duns, accts) =>
        for {
          _ <- party.change(
	    name = name,
	    orcid = orcid,
	    affiliation = affiliation.map(Maybe(_).opt),
	    duns = duns.filter(_ => request.access.direct == Permission.ADMIN))
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

  private def create(form : PartyForm, passwd : Boolean)(implicit site : Site) : Either[PartyForm, Future[SiteParty]] =
    form.fold(Left(_), {
      case (None, _, _, _, _) => Left(form.withError("name", "error.required"))
      case (_, _, _, _, Some((_, None, _, _))) => Left(form.withError("email", "error.required"))
      case (Some(name), orcid, affiliation, duns, accts) => Right {
	for {
	  p <- Party.create(
	    name = name,
	    orcid = orcid.flatten,
	    affiliation = affiliation,
	    duns = duns.flatten)
          a <- macros.Async.map[AccountMapping, Account](accts, { case (_, email, password, openid) =>
	    Account.create(p,
	      email = email.get,
	      password = password.filter(_ => passwd),
	      openid)
          })
	  s <- p.perSite
	} yield (s)
      }
    })


  def create(acct : Boolean = false) : Action[_] = RootAction.async { implicit request =>
    create(partyForm(acct).bindFromRequest, true).fold(
      ABadForm[PartyMapping](views.html.party.edit(None, _), _),
      _.map(result))
  }

  type AuthorizeMapping = (Option[String], Permission.Value, Permission.Value, Permission.Value, Boolean, Option[Date], Option[String], Boolean)
  type AuthorizeForm = Form[AuthorizeMapping]
  protected val authorizeForm : AuthorizeForm = Form(tuple(
    "name" -> optional(nonEmptyText),
    "inherit" -> default(Field.enum(Permission), Permission.NONE),
    "direct" -> default(Field.enum(Permission), Permission.NONE),
    "permission" -> default(Field.enum(Permission), Permission.NONE),
    "pending" -> boolean,
    "expires" -> optional(jodaLocalDate),
    "info" -> optional(nonEmptyText),
    "delete" -> boolean
  ))

  protected final val maxExpiration = org.joda.time.Years.years(2)

  protected def authorizeFormFill(auth : Authorize, apply : Boolean = false) : AuthorizeForm =
    authorizeForm.fill((None, auth.inherit, auth.direct, auth.permission,
      auth.authorized.isEmpty, auth.expires.map(_.toLocalDate),
      auth.info, false))

  def authorizeChange(id : models.Party.Id, childId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(childId).flatMap(_.fold(ANotFound) { child =>
    def bad(form : AuthorizeForm) =
      AbadForm[AuthorizeMapping](f => PartyHtml.viewAdmin(Seq((false, Some(child), f))), form)
    val form = authorizeForm.bindFromRequest
    form.fold(bad, {
      case (_, _, _, _, _, _, _, true) =>
	for {
	  _ <- models.Authorize.delete(childId, id)
	} yield (result(request.obj))
      case (_, inherit, direct, permission, pending, expires, info, _) =>
        val (exp, expok) = if (request.superuser) (expires, true)
          else {
            val maxexp = (new Date).plus(maxExpiration)
            val exp = expires.getOrElse(maxexp)
            (Some(exp), !exp.isAfter(maxexp))
          }
        if (!expok)
          bad(form.withError("expires", "error.max", maxExpiration))
        else for {
          _ <- Authorize.set(childId, id, max(inherit, permission), max(direct, permission), if (pending) None else Some(new Timestamp), exp.map(_.toLocalDateTime(org.joda.time.LocalTime.MIDNIGHT)))
	  _ <- Authorize.Info.set(childId, id, info)
	} yield (result(request.obj))
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

  private def delegates(party : Party) : Future[Seq[Account]] =
    party.authorizeChildren().map(_.filter(_.direct >= Permission.ADMIN).flatMap(_.child.account)
      ++ party.account)

  def authorizeApply(id : models.Party.Id, parentId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(parentId).flatMap(_.fold(ANotFound) { parent =>
    authorizeForm.bindFromRequest.fold(
      AbadForm[AuthorizeMapping](f => PartyHtml.viewAdmin(Seq((true, Some(parent), f))), _),
      { case (_, inherit, _, _, _, _, info, _) =>
	for {
	  dl <- delegates(parent)
	  _ <- Authorize.set(id, parentId, inherit, Permission.NONE, None, None)
	  _ <- Authorize.Info.set(id, parentId, info)
	  _ <- Mail.send(
	    to = (dl.map(_.email) :+ Messages("mail.authorize")).mkString(", "),
	    subject = Messages("mail.authorize.subject"),
	    body = Messages("mail.authorize.body", routes.PartyHtml.admin(parentId).absoluteURL(true))
	  ).recover {
	    case ServiceUnavailableException => ()
	  }
        } yield (result(request.obj))
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
    authorizeForms : Seq[(Boolean,Option[models.Party],AuthorizeForm)] = Nil)(
    implicit request : Request[_]) = {
    val change = authorizeForms.flatMap(_._2.map(_.id.unId)).toSet
    val search = Set(false, true) -- authorizeForms.filter(_._2.isEmpty).map(_._1)
    for {
      parents <- request.obj.party.authorizeParents(true)
      children <- request.obj.party.authorizeChildren(true)
      forms = children
        .filterNot(t => change.contains(t.childId.unId))
        .map(t => (false, Some(t.child), authorizeFormFill(t))) ++
	search.map(apply =>
	  (apply, None, authorizeForm.fill((None, Permission.NONE, Permission.NONE, Permission.NONE, true, Some((new Date).plus(maxExpiration)), None, false)))) ++
        authorizeForms
    } yield (views.html.party.authorize(parents, forms))
  }
  
  def edit(i : models.Party.Id) = AdminAction(i) { implicit request =>
    Ok(views.html.party.edit(Some(request.obj.party), formFill))
  }

  def createNew(acct : Boolean = false) = RootAction { implicit request =>
    Ok(views.html.party.edit(None, partyForm(acct)))
  }

  def admin(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    viewAdmin().map(Ok(_))
  }

  def authorizeSearch(id : models.Party.Id, apply : Boolean) = AdminAction(id).async { implicit request =>
    def bad(form : AuthorizeForm) =
      AbadForm[AuthorizeMapping](f => viewAdmin(Seq((apply, None, f))), form)
    val form = authorizeForm.bindFromRequest
    form.fold(bad _, {
      case (None, _, _, _, _, _, _, _) =>
	bad(form.withError("name", "error.required"))
      case (Some(name), _, _, _, _, _, info, true) => for {
	_ <- Mail.send(
	  to = Messages("mail.authorize"),
	  subject = Messages("mail.authorize.subject"),
	  body = Messages("mail.authorize.body", routes.PartyHtml.view(id).absoluteURL(true) + " " +
	    request.obj.party.name + " (" + request.obj.party.affiliation + ") " + name + ": " + info))
      } yield (Ok("request sent"))
      case (Some(name), _, _, _, _, _, _, _) => for {
        res <- models.Party.searchForAuthorize(name, request.obj.party)
        r <- viewAdmin((apply, None, form) +: 
          res.map(e => (apply, Some(e), form)))
      } yield (Ok(r))
    })
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
      throw new BadFormException[AvatarMapping](views.html.party.edit(Some(request.obj.party), formFill, _))(form)
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
