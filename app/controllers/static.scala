package controllers

import play.api.templates.HtmlFormat._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._

private[controllers] sealed class StaticController extends SiteController

object StaticHtml extends StaticController {
  def index() = SiteAction {
    implicit request =>
      Ok(views.html.widget.template.html5("Dashboard")(raw(""))(request))
  }
}
