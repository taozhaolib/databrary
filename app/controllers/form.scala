package controllers

import play.api.data.Forms._

object Field {
  def enum(enum : Enumeration) = number(min=0, max=enum.maxId-1).transform[enum.Value](enum(_), _.id)
}
