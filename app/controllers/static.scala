package controllers

object Static extends SiteController {
  def index() = SiteAction {
    implicit request =>
      Ok(views.html.static.index())
  }

  def about() = SiteAction {
    implicit request =>
      Ok(views.html.static.about())
  }

  def policies() = SiteAction {
    implicit request =>
      Ok(views.html.static.policies())
  }

  def board() = SiteAction {
    implicit request =>
      Ok(views.html.static.board())
  }

  def team() = SiteAction {
    implicit request =>
      Ok(views.html.static.team())
  }

  def contributors() = SiteAction {
    implicit request =>
      Ok(views.html.static.contributors())
  }

  def faq() = SiteAction {
    implicit request =>
      Ok(views.html.static.faq())
  }

  def jobs() = SiteAction {
    implicit request =>
      Ok(views.html.static.jobs())
  }

  def contact() = SiteAction {
    implicit request =>
      Ok(views.html.static.contact())
  }
}