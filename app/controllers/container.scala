package controllers

import site._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import java.sql.Date
import models._

object Container extends SiteController {
  type Request[A] = RequestObject[Volume]#Site[A]

  private[controllers] def action(v : models.Volume.Id, i : models.Container.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(v, models.Container.get(i)(_), p)

  private[controllers] def Action(v : models.Volume.Id, i : models.Container.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(v, i, p)
}
