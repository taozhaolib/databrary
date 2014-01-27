package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.data._
import site._
import dbrary._
import models._

private[controllers] sealed class TagController extends SiteController {
  type TagMapping = (Option[String], Option[Boolean])
  type TagForm = Form[TagMapping]
  val tagForm : TagForm = Form(Forms.tuple(
    "name" -> OptionMapping(Forms.nonEmptyText),
    "vote" -> Forms.optional(Forms.boolean)
  ))

  def update(name : String = "", i : models.Container.Id, segment : Segment) =
    (SiteAction.access(Permission.VIEW) ~> SlotController.action(i, segment)).async { implicit request =>
      tagForm.bindFromRequest.fold(
        AbadForm[TagMapping](f => SlotHtml.show(tagForm = f), _),
        { case (name2, vote) =>
          for {
            r <- request.obj.setTag(name2.getOrElse(name), vote)(request.asInstanceOf[AuthSite])
          } yield {
	    if (request.isApi) r.fold(BadRequest(""))(r => Ok(r.json.js))
	    else Redirect(request.obj.pageURL)
	  }
        }
      )
    }
}

object TagController extends TagController

object TagHtml extends TagController

object TagApi extends TagController {
  def get(name : String) = SiteAction.async { implicit request =>
    Tag.get(name).flatMap(_.fold(
      ANotFound)(
      _.json(request.apiOptions).map(Ok(_)))
    )
  }
}
