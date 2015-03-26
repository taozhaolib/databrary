package site

import scala.concurrent.Future
import play.api.Play.current
import play.api.i18n.Messages
import macros.async._

object Mail {
  private lazy val mailer = current.plugin[play.api.libs.mailer.MailerPlugin]
  private def getMailer = mailer.fold(throw controllers.ServiceUnavailableException)(_.instance)
  val fromAddr = current.configuration.getString("mail.from").getOrElse("<>")
  val authorizeAddr = current.configuration.getString("mail.authorize").getOrElse("authorize")
  private val fillinKey = new javax.crypto.spec.SecretKeySpec(
    current.configuration.getString("fillin.key").getOrElse("databrary").getBytes,
    "HmacSHA256")

  def available : Boolean = mailer.isDefined
  def check() { getMailer }

  def send(from : String = fromAddr, to : Seq[String], subject : String, body : String) : Future[Unit] = Future {
    getMailer.send(play.api.libs.mailer.Email(
      subject,
      from,
      to,
      bodyText = Some(body)
    ))
    ()
  }(context.foreground)

  def investigator(party : models.Party) : Future[play.api.libs.ws.WSResponse] = {
    implicit val ctx = context.foreground
    Future {
      val hmac = javax.crypto.Mac.getInstance(fillinKey.getAlgorithm)
      hmac.init(fillinKey)
      Seq(
        "id" -> party.id.toString,
        "name" -> party.name,
        "date" -> (new dbrary.Date).toString,
        "mail" -> authorizeAddr)
        .map { case (k, v) =>
          hmac.update(v.getBytes)
          (k, Seq(v))
        } :+ ("auth" -> Seq(new String(store.Hex(hmac.doFinal))))
    }.flatMap { args =>
      play.api.libs.ws.WS.url("http://databrary.org/internal/investigator.cgi")
      .post(args.toMap)
      .whenComplete {
        case scala.util.Success(r) if r.status == 200 => ()
        case scala.util.Success(r) => play.api.Logger.error("investigator registration call failed: " + r.statusText + "\n" + r.body)
        case scala.util.Failure(e) => play.api.Logger.error("investigator registration call failed", e)
      }
    }
  }
}
