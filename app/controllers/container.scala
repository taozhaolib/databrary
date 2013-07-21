package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Container extends SiteController {

  private[controllers] def check(i : models.Container.Id, p : Permission.Value = Permission.VIEW)(act : Container => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Container.get(i).fold(NotFound : Result) { c =>
      if (c.permission < p)
        Forbidden
      else
        act(c)(request)
    }
  }

}
