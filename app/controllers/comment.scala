package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.data._
import site._
import dbrary._
import models._

private[controllers] sealed class CommentController extends ObjectController[Comment] {
  def post(i : Container.Id, segment : Segment, parent : Option[Comment.Id] = None) =
    SiteAction.access(Permission.PUBLIC).andThen(SlotController.action(i, segment)).async { implicit request =>
      val form = new CommentController.SlotForm()._bind
      for {
        c <- request.obj.postComment(form.text.get, parent orElse form.parent.get)
      } yield (result(c))
    }
}

object CommentController extends CommentController {
  trait Form extends StructForm {
    val text = Field(Mappings.nonEmptyText)
    val parent = Field(OptionMapping(Forms.of[Comment.Id]))
  }
  /* annoying inheritance: */
  final class CommentForm(slot : Slot)
    extends StructForm(
      routes.CommentHtml.post(slot.containerId, slot.segment))
    with Form
  final class SlotForm(implicit request : SlotController.Request[_])
    extends AHtmlForm[SlotForm](
      routes.CommentHtml.post(request.obj.containerId, request.obj.segment),
      f => SlotHtml.show(commentForm = Some(f)))
    with Form
}

object CommentHtml extends CommentController with HtmlController

object CommentApi extends CommentController with ApiController
