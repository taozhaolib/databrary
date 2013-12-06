package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._

object Static extends SiteController {
  def index() = SiteAction {
    implicit request =>
      Ok(views.html.static.index(request))
  }

  def team() = SiteAction.async { implicit request =>
    for {
      v <- models.Volume.Databrary
      a <- v.partyAccess
    } yield (Ok(views.html.static.team(a)))
  }

  def page(page : String) = SiteAction { implicit request =>
    views.html.static.pages.get(page).fold[SimpleResult](NotFound)(page =>
      if (isAjax) Ok(page.template.render)
      else Ok(views.html.widget.template.static(page.name)(page.template.render)))
  }
}
