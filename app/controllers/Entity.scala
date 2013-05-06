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

object Entity extends Controller {

  def viewEntity(a : Option[Account], e : Entity) = Ok(views.html.entity(a, e))

  def view(i : Int) = Action { request =>
    var e = models.Entity.get(i)
    if (e == null)
      NotFound
    else
      viewEntity(AccountAction.getAccount(request), e)
  }

}
