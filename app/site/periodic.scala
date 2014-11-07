package site

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object periodic {
  private implicit val executionContext = context.background
  private val logger = play.api.Logger("periodic")

  private def every(period : FiniteDuration, offset : FiniteDuration = Duration.Zero)(task : => Unit) {
    val int = period.toMillis
    val off = offset.toMillis
    val now = System.currentTimeMillis - off
    val delay = int - now % int
    // Unfortunately Scheduler always runs all tasks on shutdown, so we want to avoid that:
    var next = now + delay
    play.api.libs.concurrent.Akka.system(play.api.Play.current).scheduler.schedule(
      Duration(delay, MILLISECONDS), period) {
        val now = System.currentTimeMillis - off
        if (now >= next) {
          next = next + int
          task
        }
      }
  }

  def start() {
    every(1.days, 7.hours) {
      logger.info("Running daily cleanup...")
      for {
        _ <- models.Token.clean
        _ <- models.Volume.updateIndex
      } yield ()
    }
  }
}
