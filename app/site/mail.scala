package site

import scala.concurrent.Future
import play.api.Play.current
import play.api.i18n.Messages

object Mail {
  private lazy val mailer = current.plugin[com.typesafe.plugin.MailerPlugin]
  private def getMailer = mailer.getOrElse(throw controllers.ServiceUnavailableException)
  val fromAddr = current.configuration.getString("mail.from").getOrElse("<>")
  val authorizeAddr = current.configuration.getString("mail.authorize").getOrElse("authorize")

  def available : Boolean = mailer.isDefined
  def check() { getMailer }

  def send(from : String = fromAddr, to : Seq[String], subject : String, body : String) : Future[Unit] = Future {
    val mail = getMailer.email
    mail.setFrom(from)
    mail.setRecipient(to : _*)
    mail.setSubject(subject)
    mail.send(body)
  }(context.process)
}
