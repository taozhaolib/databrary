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

  private[this] def post(node : Commented with SitePage)(request : SiteRequest[_]) = request match {
    /* FIXME: poorly displayed and possibly untranslated error message: */
    case urequest : UserRequest[_] if urequest.access >= Permission.COMMENT =>
      form.bindFromRequest()(request).fold(
        form => BadRequest(form.errors.head.message), 
        { text =>
          node.postComment(text)(urequest)
          Redirect(node.pageURL)
        }
      )
    case _ => Forbidden
  }

  def postVolume(s : models.Volume.Id) = Volume.check(s, Permission.COMMENT)(post _)
  def postSlot(s : models.Slot.Id) = Slot.check(s, Permission.COMMENT)(post _)
}
