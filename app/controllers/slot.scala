package controllers

import site._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import          libs.json._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dbrary._
import models._

private[controllers] sealed class SlotController extends ObjectController[AbstractSlot] {
  private[controllers] def action(i : models.Slot.Id, start : Option[Offset] = None, end : Option[Offset] = None, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(models.Slot.get(i, Range[Offset](start, end))(_), p)

  private[controllers] def Action(i : models.Slot.Id, start : Option[Offset] = None, end : Option[Offset] = None, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(i, start, end, p)

  type EditForm = Form[(Option[(Option[String], Option[Date])], Consent.Value)]
  protected def editForm(container : Boolean) : EditForm = Form(tuple(
    "" -> MaybeMapping(if (container) Some(tuple(
      "name" -> optional(nonEmptyText),
      "date" -> optional(jodaLocalDate)
    )) else None),
    "consent" -> Field.enum(Consent)
  ))
  protected def editFormFill(s : Slot) = {
    val full = s.isFull
    val cont = (if (full) Some(s.container) else None)
    editForm(full).fill((cont.map(c => (c.name, c.date)), s.consent))
  }

  def formForContainer(form : EditForm, slot : Slot) =
    form.value.fold(slot.isFull)(_._1.isDefined)

  type CommentMapping = (String, Option[Comment.Id])
  type CommentForm = Form[CommentMapping]
  val commentForm : CommentForm = Form(tuple(
    "text" -> nonEmptyText,
    "parent" -> OptionMapping(of[Comment.Id])
  ))

  def comment(i : models.Slot.Id, start : Option[Offset], end : Option[Offset], parent : Option[Comment.Id] = None) =
    (SiteAction.access(Permission.VIEW) ~> action(i, start, end)).async { implicit request =>
      commentForm.bindFromRequest.fold(
        AbadForm[CommentMapping](f => SlotHtml.show(commentForm = f), _),
        { case (text, parent2) =>
          for {
            _ <- request.obj.postComment(text, parent orElse parent2)(request.asInstanceOf[AuthSite])
          } yield (result(request.obj))
        }
      )
    }

  type TagMapping = (Option[String], Option[Boolean])
  type TagForm = Form[TagMapping]
  val tagForm : TagForm = Form(tuple(
    "name" -> OptionMapping(nonEmptyText),
    "vote" -> optional(boolean)
  ))

  def tag(i : models.Slot.Id, start : Option[Offset], end : Option[Offset], name : String = "") =
    (SiteAction.access(Permission.VIEW) ~> action(i, start, end)).async { implicit request =>
      tagForm.bindFromRequest().fold(
        AbadForm[TagMapping](f => SlotHtml.show(tagForm = f), _),
        { case (name2, vote) =>
          for {
            _ <- request.obj.setTag(name2.getOrElse(name), vote)(request.asInstanceOf[AuthSite])
          } yield (result(request.obj))
        }
      )
    }
}

object SlotHtml extends SlotController {
  private[controllers] def actionId(v : models.Volume.Id, i : models.Slot.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(v, models.Slot.get(i)(_), p)

  private[controllers] def ActionId(v : models.Volume.Id, i : models.Slot.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> actionId(v, i, p)

  private[controllers] def show(commentForm : CommentForm = commentForm, tagForm : TagForm = tagForm)(implicit request : Request[_]) = {
    val slot = request.obj
    for {
      records <- slot.records
      assets <- slot.assets
      comments <- slot.comments
      tags <- slot.tags
    } yield (views.html.slot.view(records, assets, comments, commentForm, tags, tagForm))
  }

  def view(i : models.Slot.Id, start : Option[Offset] = None, end : Option[Offset] = None) = Action(i, start, end).async { implicit request =>
    if (request.obj.isTop)
      ARedirect(controllers.routes.VolumeHtml.view(request.obj.volumeId))
    else
      show().map(Ok(_))
  }

  private[controllers] def viewEdit(status : Status, slot : Slot)(
    editForm : EditForm = editFormFill(slot),
    recordForm : RecordHtml.SelectForm = RecordHtml.selectForm)(
    implicit request : Request[_]) = {
    RecordHtml.selectList(slot).map { selectList =>
      status(views.html.slot.edit(Right(slot), editForm, Some(recordForm), selectList))
    }
  }

  def edit(v : models.Volume.Id, i : models.Slot.Id) = ActionId(v, i, Permission.EDIT).async { implicit request =>
    viewEdit(Ok, request.obj)()
  }

  def createContainer(v : models.Volume.Id) = VolumeController.Action(v, Permission.EDIT) { implicit request =>
    Ok(views.html.slot.edit(Left(request.obj), editForm(true), None))
  }

  def change(v : models.Volume.Id, i : models.Slot.Id) = ActionId(v, i, Permission.EDIT).async { implicit request =>
    editFormFill(request.obj).bindFromRequest.fold(
      form => viewEdit(BadRequest, request.obj)(editForm = form),
      { case (container, consent) =>
        for {
          _ <- macros.Async.map[(Option[String], Option[Date]), Boolean](container, {
            case (name, date) => request.obj.container.change(name = Some(name), date = Some(date))
          })
          _ <- request.obj.setConsent(consent)
        } yield (Redirect(request.obj.pageURL))
      }
    )
  }

  def addContainer(s : models.Volume.Id) = VolumeController.Action(s, Permission.CONTRIBUTE).async { implicit request =>
    val form = editForm(true).bindFromRequest
    form.fold(
      form => ABadRequest(views.html.slot.edit(Left(request.obj), form, None)),
    { case (Some((name, date)), consent) =>
        for {
          cont <- models.Container.create(request.obj, name = name, date = date)
          _ <- cont.setConsent(consent)
        } yield (Redirect(cont.pageURL))
      case _ => ABadRequest(views.html.slot.edit(Left(request.obj), form, None))
    })
  }

  type CreateForm = Form[(Option[Offset], Option[Offset])]
  private[this] val createForm : CreateForm = Form(tuple(
    "start" -> optional(of[Offset]),
    "end" -> optional(of[Offset])
  ).verifying(Messages("range.invalid"), !_.zipped.exists(_ > _)))

  def create(v : models.Volume.Id, c : models.Slot.Id) = ActionId(v, c, Permission.CONTRIBUTE) { implicit request =>
    Ok(views.html.slot.create(createForm))
  }

  def add(v : models.Volume.Id, c : models.Slot.Id) = ActionId(v, c, Permission.CONTRIBUTE).async { implicit request =>
    createForm.bindFromRequest.fold(
      form => ABadRequest(views.html.slot.create(form)),
      { case (start, end) =>
        models.Slot.getOrCreate(request.obj.container, Range[Offset](start, end).map(request.obj.position + _)).map { slot =>
          Redirect(slot.pageURL)
        }
      }
    )
  }

  def thumb(v : models.Volume.Id, s : models.Slot.Id) = ActionId(v, s, Permission.VIEW).async { implicit request =>
    request.obj.thumb.flatMap(_.fold(
      Assets.at("/public", "images/draft.png")(request))(
      a => SlotAsset.getFrame(Left(0.25f))(request.withObj(a))))
  }
}

object SlotApi extends SlotController {
  def get(c : models.Container.Id, start : Option[Offset], end : Option[Offset]) = Action(c, start, end).async { request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }
}
