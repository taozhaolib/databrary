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

  def postStudy(s : models.Study.Id) = Study.check(s) { study => implicit request =>
    form.bindFromRequest.fold(
      form => BadRequest(form.errors.head.message),
      text => { study.addComment(text) ; Redirect(routes.Study.view(s)) }
    )
  }

  def postObjectLink(c : models.Container.Id, o : models.Object.Id) = Object.check(c, o) { link => implicit request =>
    form.bindFromRequest.fold(
      form => BadRequest(form.errors.head.message),
      text => { link.addComment(text) ; Redirect(routes.Object.view(link.containerId, link.objId)) }
    )
  }
}
