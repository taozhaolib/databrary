package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          db.slick.DB
import          db.slick.Config.driver.simple._
import          i18n.Messages
import models._

object Study extends Controller {

  def viewStudy(s : Study)(implicit request : SiteRequest[_]) = {
    val access = s.check_access(request.identity.id)
    if (access < Permission.VIEW)
      Forbidden
    else
      Ok(views.html.study(s, access))
  }

  def view(i : Int) = SiteAction { implicit request =>
    models.Study.get(i) match {
      case None => NotFound
      case Some(s) => viewStudy(s)
    }
  }

  def viewable(e : Int)(implicit request : SiteRequest[_]) = DB.withSession { implicit session =>
    val l = for { 
      a <- StudyAccess.byEntity(e, Permission.CONTRIBUTE) 
      if StudyAccess.filterForEntity(request.identity.id)(a.studyId)
      s <- a.study
    } yield (s)
    l.list
  }

  def checkEdit(i : Int)(act : Study => AccountRequest[AnyContent] => Result) = AccountAction { request =>
    if (StudyAccess.check(request.account.id, i) < Permission.EDIT)
      Forbidden
    else
      act(models.Study.get(i).get)(request)
  }

  def edit(i : Int) = checkEdit(i) { study => implicit request =>
    NotFound
  }

}
