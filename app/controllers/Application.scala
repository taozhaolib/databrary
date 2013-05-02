package controllers

import play.api._
import          Play.current
import          mvc._
import          i18n.Messages
import models._

object Application extends Controller {
  
  def start = AccountAction(Login.viewLogin,
    { request : AccountRequest[AnyContent] => Account.viewHome(request) })

  def ddl = Action {
    Ok(views.html.ddl(models.Entity.ddl ++ models.Trust.ddl ++ models.Account.ddl))
  }

}
