package controllers

import play.api.libs.ws.WS
import scala.concurrent.Future
import play.api._
import          Play.current
import          http._
import          mvc._
import          data._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import macros._
import macros.async._
import dbrary._
import site._
import models._


class EZIDController extends Controller {
  protected def AOk[C : Writeable](c : C) : Future[Result] = async(Ok[C](c))
  protected def ABadRequest[C : Writeable](c : C) : Future[Result] = async(BadRequest[C](c))
  protected def ARedirect(c : Call) : Future[Result] = async(Redirect(c))
  protected def ANotFound(implicit request : SiteRequest[_]) : Future[Result] =
    NotFoundException.result
}

object APIConn extends EZIDController {

  final val baseUrl: String = "https://ezid.cdlib.org"
  private val apiUser: String = "apitest"
  private val apiPass: String = "apitest"

  def c = new EZIDClient

  def res = c.statusRequest(this.baseUrl, this.apiPass, this.apiUser, "/status")
  def idData = c.idCheck(this.baseUrl, this.apiPass, this.apiUser, "/id/", ezid: String) /** trying to pull something like https://ezid.cdlib.org/id/ark:/99999/fk4n018k4j through routes*/

}


class EZIDClient {

  def statusRequest(urlstring: String, p: String, u: String, requested: String) = Action.async{

    WS.url(urlstring+requested).get.map(HTTP.wsResult)

  }

  def idCheck(urlstring: String, p: String, u: String, requested: String, ezid: String) = Action.async{

    WS.url(urlstring+requested+ezid).get.map(HTTP.wsResult)

  }

}


