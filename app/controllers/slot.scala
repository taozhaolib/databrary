package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import          libs.json._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import site._
import dbrary._
import models._

private[controllers] sealed class SlotController extends ObjectController[AbstractSlot] {
  private[controllers] def action(i : Slot.Id, segment : Segment, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(Slot.get(i, segment)(_), p)

  private[controllers] def Action(i : Slot.Id, segment : Segment, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(i, segment, p)

  type EditMapping = (Option[(Option[String], Option[Date])], Consent.Value)
  type EditForm = Form[EditMapping]
  protected def editForm(container : Boolean) : EditForm = Form(tuple(
    "" -> MaybeMapping(if (container) Some(tuple(
      "name" -> optional(nonEmptyText),
      "date" -> optional(jodaLocalDate)
    )) else None),
    "consent" -> Field.enum(Consent)
  ))
  protected def editFormFill(s : AbstractSlot) = {
    val full = s.isFull
    val cont = (if (full) Some(s.container) else None)
    editForm(full).fill((cont.map(c => (c.name, c.date)), s.consent))
  }

  def formForContainer(form : EditForm, slot : AbstractSlot) =
    form.value.fold(slot.isFull)(_._1.isDefined)

  def update(i : Slot.Id, segment : Segment) = Action(i, segment, Permission.EDIT).async { implicit request =>
    editFormFill(request.obj).bindFromRequest.fold(
      AbadForm[EditMapping](f => SlotHtml.viewEdit(request.obj)(editForm = f), _),
      { case (container, consent) =>
        for {
          _ <- macros.Async.map[(Option[String], Option[Date]), Boolean](container, {
            case (name, date) => request.obj.container.change(name = Some(name), date = Some(date))
          })
          _ <- request.obj.setConsent(consent)
        } yield (result(request.obj))
      }
    )
  }

  def thumb(i : models.Slot.Id, segment : Segment) = Action(i, segment, Permission.VIEW).async { implicit request =>
    request.obj.thumb.flatMap(_.fold(
      Assets.at("/public", "images/draft.png")(request))(
      a => SlotAssetHtml.getFrame(Left(0.25f))(request.withObj(a))))
  }

  type CommentMapping = (String, Option[Comment.Id])
  type CommentForm = Form[CommentMapping]
  val commentForm : CommentForm = Form(tuple(
    "text" -> nonEmptyText,
    "parent" -> OptionMapping(of[Comment.Id])
  ))

  def comment(i : Slot.Id, segment : Segment, parent : Option[Comment.Id] = None) =
    (SiteAction.access(Permission.VIEW) ~> action(i, segment)).async { implicit request =>
      commentForm.bindFromRequest.fold(
        AbadForm[CommentMapping](f => SlotHtml.show(commentForm = f), _),
        { case (text, parent2) =>
          for {
            _ <- request.obj.postComment(text, parent orElse parent2)(request.asInstanceOf[AuthSite])
          } yield (result(request.obj))
        }
      )
    }
}

object SlotController extends SlotController

object SlotHtml extends SlotController {
  private[controllers] def show(commentForm : CommentForm = commentForm, tagForm : TagHtml.TagForm = TagHtml.tagForm)(implicit request : Request[_]) = {
    val slot = request.obj
    for {
      records <- slot.records
      assets <- slot.assets
      comments <- slot.comments
      tags <- slot.tags
    } yield (views.html.slot.view(records, assets, comments, commentForm, tags, TagHtml.tagForm))
  }

  def view(i : models.Slot.Id, segment : Segment) = Action(i, segment).async { implicit request =>
    if (request.obj.isTop)
      ARedirect(controllers.routes.VolumeHtml.view(request.obj.volumeId))
    else
      show().map(Ok(_))
  }

  private[controllers] def viewEdit(slot : AbstractSlot)(
    editForm : EditForm = editFormFill(slot),
    recordForm : RecordHtml.SelectForm = RecordHtml.selectForm)(
    implicit request : Request[_]) =
    for {
      records <- slot.records
      selectList <- RecordHtml.selectList(slot)
    } yield (views.html.slot.edit(Right(slot), editForm, records, Some(recordForm), selectList))

  def edit(i : models.Slot.Id, segment : Segment) = Action(i, segment, Permission.EDIT).async { implicit request =>
    viewEdit(request.obj)().map(Ok(_))
  }

  def createContainer(v : models.Volume.Id) = VolumeController.Action(v, Permission.EDIT) { implicit request =>
    Ok(views.html.slot.edit(Left(request.obj), editForm(true), Nil, None))
  }

  def addContainer(s : models.Volume.Id) = VolumeController.Action(s, Permission.CONTRIBUTE).async { implicit request =>
    val form = editForm(true).bindFromRequest
    form.fold(
      form => ABadRequest(views.html.slot.edit(Left(request.obj), form, Nil, None)),
    { case (Some((name, date)), consent) =>
        for {
          cont <- models.Container.create(request.obj, name = name, date = date)
          _ <- cont.setConsent(consent)
        } yield (Redirect(cont.pageURL))
      case _ => ABadRequest(views.html.slot.edit(Left(request.obj), form, Nil, None))
    })
  }
}

object SlotApi extends SlotController {
  def get(c : models.Container.Id, segment : Segment) = Action(c, segment).async { request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }
}
