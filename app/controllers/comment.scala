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

  def postStudyObject(s : models.Study.Id, o : models.Object.Id) = Object.check(s, o) { obj => implicit request =>
    form.bindFromRequest.fold(
      form => BadRequest(form.errors.head.message),
      text => { obj.addComment(text) ; Redirect(routes.Object.view(obj.studyId, obj.objId)) }
    )
  }
}
