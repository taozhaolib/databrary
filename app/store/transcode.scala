package store

import java.io.File
import scala.concurrent.{Future,ExecutionContext,Await,duration}
import scala.sys.process.Process
import play.api.Play.current
import play.api.libs.Files.TemporaryFile
import play.api.mvc.RequestHeader
import macros._
import macros.async._
import dbrary._
import site._

object Transcode {
  private implicit def context : ExecutionContext = site.context.background
  private val logger = play.api.Logger("transcode")
  private val host : Option[String] = current.configuration.getString("transcode.host").flatMap(Maybe(_).opt())
  private val dir : Option[File] = current.configuration.getString("transcode.dir").flatMap(Maybe(_).opt()).map { s =>
    new File(s)
  }

  def enabled = dir.exists(d => host.isDefined || d.isDirectory)

  private def procLogger(prefix : String) = {
    val pfx = if (prefix.nonEmpty) prefix + ": " else prefix
    scala.sys.process.ProcessLogger(
      s => logger.info(pfx + s),
      s => logger.warn(pfx + s))
  }

  private lazy val ctlCmd : Seq[String] = {
    val ctl = current.getExistingFile("conf/transctl.sh")
      .getOrElse(throw new RuntimeException("conf/transctl.sh not found"))
    ctl.setExecutable(true)
    ctl.getPath +: (Seq("-v", Site.version) ++
      (dir : Iterable[File]).flatMap(d => Seq("-d", d.getPath)) ++
      (host : Iterable[String]).flatMap(Seq("-h", _)))
  }

  /* Despite the fact that this is a future, it's actually blocking in the background context. */
  private def ctl(id : models.Transcode.Id, args : String*) : Future[String] = Future {
    val cmd = ctlCmd ++ Seq("-i", id.toString) ++ args
    logger.debug(cmd.mkString(" "))
    Process(cmd).!!(procLogger(id.toString)).trim // XXX blocking, should be futurable
  }

  def start(id : models.Transcode.Id, args : Seq[String]) : Future[Int] =
    ctl(id, args : _*)
    .map(_.toInt)
    .whenComplete { r =>
      logger.debug("running " + id + ": " + r.toEither.merge.toString)
    }

  def stop(id : models.Transcode.Id, pid : Int) : Future[String] =
    ctl(id, "-k", pid.toString)

  def collect(id : models.Transcode.Id, res : Int, log : String) : Future[TemporaryFile] = {
    logger.info("result " + id + (if (res != 0) " exit " + res else "") + ": " + log)
    val o = TemporaryFile(Upload.file(id + ".mp4"))
    if (res != 0) Future.failed(new RuntimeException("exit " + res))
    else ctl(id, "-c", o.file.getAbsolutePath).map(_ => o)
      .whenFailure { case e : Exception =>
        logger.error("collecting " + id, e)
      }
  }
}
