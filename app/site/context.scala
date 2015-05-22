package site

import scala.concurrent._
import play.api.libs.concurrent._
import play.api.Play.current

object context {
  /** Generic p1 processing thread for serving user requests that require additional, potentially longer computation. */
  val foreground : ExecutionContext = Akka.system.dispatchers.lookup("foreground")
  /** Single-thread for admin and background processes that should be serialized and throttled. */
  val background : ExecutionContext = Akka.system.dispatchers.lookup("background")
}
