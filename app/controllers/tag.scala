package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.data._
import site._
import dbrary._
import models._

private[controllers] sealed class TagController extends SiteController {
  def update(name : String = "", i : models.Container.Id, segment : Segment) =
    (SiteAction.access(Permission.VIEW) ~> SlotController.action(i, segment)).async { implicit request =>
      val form = new TagController.SlotForm()._bind
      for {
	r <- request.obj.setTag(form.name.get getOrElse name, form.vote.get)(request.asInstanceOf[AuthSite])
      } yield {
	if (request.isApi) r.fold(BadRequest(""))(r => Ok(r.json.js))
	else Redirect(request.obj.pageURL)
      }
    }
}

object TagController extends TagController {
  trait Form extends StructForm {
    val name = Field(OptionMapping(Forms.nonEmptyText))
    val vote = Field(Forms.optional(Forms.boolean))
  }
  /* annoying inheritance: */
  final class TagForm(slot : Slot)
    extends StructForm(
      routes.TagHtml.update("", slot.containerId, slot.segment))
    with Form
  final class SlotForm(implicit request : SlotController.Request[_])
    extends AHtmlForm[SlotForm](
      routes.TagHtml.update("", request.obj.containerId, request.obj.segment),
      f => SlotHtml.show(tagForm = Some(f)))
    with Form
}

object TagHtml extends TagController with HtmlController

object TagApi extends TagController with ApiController {
  def get(name : String) = SiteAction.async { implicit request =>
    Tag.get(name).flatMap(_.fold(
      ANotFound)(
      _.json(request.apiOptions).map(Ok(_)))
    )
  }

  def search(name : String) = SiteAction.async { implicit request =>
    Tag.search(name).map(l => Ok(JsonArray.map[Tag, String](_.name)(l)))
  }
}
