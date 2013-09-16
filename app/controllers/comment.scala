package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Comment extends SiteController {

  type CommentForm = Form[String]
  val form = Form("text" -> nonEmptyText)

  private[this] def post(node : Annotated with SitePage)(request : SiteRequest[_]) =
    if (request.access < Permission.COMMENT)
      Forbidden : Result
    else
      /* FIXME: poorly displayed and possibly untranslated error message: */
      form.bindFromRequest()(request).fold(form => BadRequest(form.errors.head.message), { text =>
        node.postComment(text)(request)
        Redirect(node.pageURL)
      })

  def postVolume(s : models.Volume.Id) = Volume.check(s, Permission.COMMENT)(post _)
  def postSlot(s : models.Slot.Id) = Slot.check(s, Permission.COMMENT)(post _)
}
