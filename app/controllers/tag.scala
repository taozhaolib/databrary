package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.data._
import site._
import dbrary._
import models._

private[controllers] sealed class TagController extends SiteController {
  def update(name : String = "", i : models.Container.Id, segment : Segment) =
    SiteAction.access(Permission.VIEW).andThen(SlotController.action(i, segment)).async { implicit request =>
      val form = new TagController.SlotForm()._bind
      val tag = form.name.get orElse TagName.validate(name) getOrElse form.name.withError("tag.invalid")._throw
      if (form.keyword.get && !request.obj.checkPermission(Permission.EDIT))
        throw ForbiddenException
      tag.set(request.obj, form.vote.get, form.keyword.get)
      .flatMap(_.fold(form.vote.withError("error.conflict")._throw) { t =>
        if (request.isApi)
          t.coverage(request.obj.container).map(t => Ok((t.tag.json ++ t.json).js))
        else
          ARedirect(request.obj.pageURL)
      })
    }
}

object TagController extends TagController {
  trait Form extends StructForm {
    val name = Field(OptionMapping(Mappings.tag))
    val vote = Field(Forms.boolean)
    val keyword = Field(Forms.boolean).fill(false)
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

  def top() = SiteAction.async { implicit request =>
    TagWeight.getAll().map(l => Ok(JsonArray.map[TagWeight, JsonRecord](w => w.tag.json ++ w.json)(l)))
  }
}
