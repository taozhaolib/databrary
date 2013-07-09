package controllers

import play.api.data.Forms._

object form {
  def enumField(enum : Enumeration) = number(min=0, max=enum.maxId-1).transform[enum.Value](enum(_), _.id)
}
