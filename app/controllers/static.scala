package controllers

object Static extends SiteController {
  def index() = SiteAction {
    implicit request =>
      Ok(views.html.static.index(request))
  }

  def about() = SiteAction {
    implicit request =>
      Ok(views.html.static.about(request))
  }

  def policies() = SiteAction {
    implicit request =>
      Ok(views.html.static.policies(request))
  }

  def board() = SiteAction {
    implicit request =>
      Ok(views.html.static.board(request))
  }

  def team() = SiteAction {
    implicit request =>
      Ok(views.html.static.team(request))
  }

  def contributors() = SiteAction {
    implicit request =>
      Ok(views.html.static.contributors(request))
  }

  def faq() = SiteAction {
    implicit request =>
      Ok(views.html.static.faq(request))
  }

  def jobs() = SiteAction {
    implicit request =>
      Ok(views.html.static.jobs(request))
  }

  def contact() = SiteAction {
    implicit request =>
      Ok(views.html.static.contact(request))
  }
}