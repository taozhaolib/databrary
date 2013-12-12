package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.json
import macros._

object Field {
  def enum(enum : Enumeration, maxId : Option[Int] = None) = number(min=0, max=maxId.getOrElse(enum.maxId-1)).transform[enum.Value](enum(_), _.id)
}
