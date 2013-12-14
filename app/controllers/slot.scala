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

package object Slot extends ObjectController[Slot] {
  private[controllers] def action(v : models.Volume.Id, i : models.Slot.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(v, models.Slot.get(i)(_), p)

  private[controllers] def Action(v : models.Volume.Id, i : models.Slot.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(v, i, p)

  def view(v : models.Volume.Id, i : models.Slot.Id) = Action(v, i).async { implicit request =>
    val slot = request.obj
    if (slot.isFull && slot.container.top)
      ARedirect(controllers.Volume.routes.html.view(slot.volumeId))
    else for {
      records <- slot.records
      assets <- slot.assets
      comments <- slot.comments(true)
    } yield (Ok(views.html.slot.view(records, assets, comments)))
  }

  type EditForm = Form[(Option[(Option[String], Option[Date])], Consent.Value)]
  private[this] def editForm(container : Boolean) : EditForm = Form(tuple(
    "" -> MaybeMapping(if (container) Some(tuple(
      "name" -> optional(nonEmptyText),
      "date" -> optional(jodaLocalDate)
    )) else None),
    "consent" -> Field.enum(Consent)
  ))
  private[this] def editFormFill(s : Slot) = {
    val full = s.isFull
    val cont = (if (full) Some(s.container) else None)
    editForm(full).fill((cont.map(c => (c.name, c.date)), s.consent))
  }

  def formForContainer(form : EditForm, slot : Slot) =
    form.value.fold(slot.isFull)(_._1.isDefined)

  private[controllers] def viewEdit(status : Status, slot : Slot)(
    editForm : EditForm = editFormFill(slot),
    recordForm : Record.SelectForm = Record.selectForm)(
    implicit request : Request[_]) = {
    Record.selectList(slot).map { selectList =>
      status(views.html.slot.edit(Right(slot), editForm, Some(recordForm), selectList))
    }
  }

  def edit(v : models.Volume.Id, i : models.Slot.Id) = Action(v, i, Permission.EDIT).async { implicit request =>
    viewEdit(Ok, request.obj)()
  }

  def createContainer(v : models.Volume.Id) = Volume.Action(v, Permission.EDIT) { implicit request =>
    Ok(views.html.slot.edit(Left(request.obj), editForm(true), None))
  }

  def change(v : models.Volume.Id, i : models.Slot.Id) = Action(v, i, Permission.EDIT).async { implicit request =>
    editFormFill(request.obj).bindFromRequest.fold(
      form => viewEdit(BadRequest, request.obj)(editForm = form),
      { case (container, consent) =>
        for {
          _ <- macros.Async.map[(Option[String], Option[Date]), Boolean](container, {
            case (name, date) => request.obj.container.change(name = name, date = date)
          })
          _ <- request.obj.setConsent(consent)
        } yield (Redirect(request.obj.pageURL))
      }
    )
  }

  def addContainer(s : models.Volume.Id) = Volume.Action(s, Permission.CONTRIBUTE).async { implicit request =>
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

  def create(v : models.Volume.Id, c : models.Slot.Id) = Action(v, c, Permission.CONTRIBUTE) { implicit request =>
    Ok(views.html.slot.create(createForm))
  }

  def add(v : models.Volume.Id, c : models.Slot.Id) = Action(v, c, Permission.CONTRIBUTE).async { implicit request =>
    createForm.bindFromRequest.fold(
      form => ABadRequest(views.html.slot.create(form)),
      { case (start, end) =>
        models.Slot.getOrCreate(request.obj.container, Range[Offset](start, end).map(request.obj.position + _)).map { slot =>
          Redirect(slot.pageURL)
        }
      }
    )
  }

  type CommentForm = Form[String]
  val commentForm : CommentForm = Form("text" -> nonEmptyText)

  def comment(v : models.Volume.Id, s : models.Slot.Id, parent : Option[models.Comment.Id]) = (SiteAction.access(Permission.VIEW) ~> action(v, s)) { implicit request =>
    commentForm.bindFromRequest().fold(
      form => Redirect(request.obj.pageURL), // not really critical enough to: BadRequest(views.html.slot.view(request.obj, form)),
      { text =>
        request.obj.postComment(text, parent)(request.asInstanceOf[AuthSite])
        Redirect(request.obj.pageURL)
      }
    )
  }

  type TagForm = Form[(String, Option[Boolean])]
  val tagForm : TagForm = Form(tuple(
    "name" -> nonEmptyText,
    "vote" -> optional(boolean)
  ))

  def tag(v : models.Volume.Id, s : models.Slot.Id) = (SiteAction.access(Permission.VIEW) ~> action(v, s)).async { implicit request =>
    tagForm.bindFromRequest().fold(
      form => ABadRequest(""/* FIXME */),
      { case (name, vote) =>
        request.obj.setTag(name, vote)(request.asInstanceOf[AuthSite]).map { _ =>
          Redirect(request.obj.pageURL)
        }
      }
    )
  }

  def thumb(v : models.Volume.Id, s : models.Slot.Id) = Action(v, s, Permission.VIEW).async { implicit request =>
    request.obj.thumb.flatMap(_.fold(
      Assets.at("/public", "images/draft.png")(request))(
      a => SlotAsset.getFrame(Left(0.25f))(request.withObj(a))))
  }

  object api {
    def tag(v : models.Volume.Id, s : models.Slot.Id) = (SiteAction.access(Permission.VIEW) ~> Slot.action(v, s)).async { implicit request =>
      Slot.tagForm.bindFromRequest().fold(
        form => ABadRequest(""),
        { case (name, vote) =>
          for {
            _ <- request.obj.setTag(name, vote)(request.asInstanceOf[AuthSite])
            tags <- request.obj.tags(true)
          } yield (Ok(JsonRecord.seq(tags.map(_.json))))
        }
      )
    }
  }
}
