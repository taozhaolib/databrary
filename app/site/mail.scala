package site

import scala.concurrent.Future
import play.api.Play.current
import play.api.i18n.Messages

object Mail {
  private lazy val mailer = current.plugin[com.typesafe.plugin.MailerPlugin]
  private def getMailer = mailer.getOrElse(throw controllers.ServiceUnavailableException)
  val fromAddr = current.configuration.getString("mail.from").getOrElse("<>")
  val authorizeAddr = current.configuration.getString("mail.authorize").getOrElse("authorize")
  private val fillinKey = new javax.crypto.spec.SecretKeySpec(
    current.configuration.getString("fillin.key").getOrElse("databrary").getBytes,
    "HmacSHA256")

  def available : Boolean = mailer.isDefined
  def check() { getMailer }

  def send(from : String = fromAddr, to : Seq[String], subject : String, body : String) : Future[Unit] = Future {
    val mail = getMailer.email
    mail.setFrom(from)
    mail.setRecipient(to : _*)
    mail.setSubject(subject)
    mail.send(body)
  }(context.process)

  def investigator(party : models.Party) : Future[play.api.libs.ws.Response] = {
    implicit val ctx = context.process
    Future {
      val hmac = javax.crypto.Mac.getInstance(fillinKey.getAlgorithm)
      hmac.init(fillinKey)
      Seq(
	"name" -> party.name,
	"date" -> (new dbrary.Date).toString,
	"mail" -> authorizeAddr)
	.map { case (k, v) =>
	  hmac.update(v.getBytes)
	  (k, Seq(v))
	} :+ ("auth" -> Seq(new String(store.Hex(hmac.doFinal))))
    }.flatMap { args =>
      val ws = play.api.libs.ws.WS.url("http://databrary.org/internal/investigator.cgi")
	.post(args.toMap)
      ws.onComplete {
	case scala.util.Success(r) if r.status == 200 => ()
	case scala.util.Success(r) => play.api.Logger.error("investigator registration call failed: " + r.statusText + "\n" + r.body)
	case scala.util.Failure(e) => play.api.Logger.error("investigator registration call failed", e)
      }
      ws
    }
  }
}
