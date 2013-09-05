package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Record extends SiteController {

  def view(i : models.Record.Id) = SiteAction { implicit request =>
    models.Record.get(i).fold(NotFound : Result) { record =>
      val l = record.containers
      if (l.isEmpty)
        NotFound
      else
        Ok(views.html.record(record, l, l.map(_.permission).max))
    }
  }

}
