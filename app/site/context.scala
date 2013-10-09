package site

import scala.concurrent._
import play.api.libs.concurrent._
import play.api.Play.current

object context {
  val main : ExecutionContext = Execution.defaultContext
  /** Generic p1 processing thread for serving requests that require additional, potentially longer computation. */
  val process : ExecutionContext = Akka.system.dispatchers.lookup("process")
}
