package store

import java.io.File
import scala.concurrent.{Future,ExecutionContext,Await,duration}
import scala.sys.process.Process
import play.api.Play.current
import play.api.libs.Files.TemporaryFile
import macros._
import macros.async._
import dbrary._
import site._

object Transcode {
  private implicit val context : ExecutionContext = play.api.libs.concurrent.Akka.system.dispatchers.lookup("transcode")
  private val logger = play.api.Logger("transcode")
  private val host : Option[String] = current.configuration.getString("transcode.host").flatMap(Maybe(_).opt)
  private val dir : Option[File] = current.configuration.getString("transcode.dir").flatMap(Maybe(_).opt).map { s =>
    val f = new File(s)
    f.mkdir
    f
  }

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

  private def ctl(id : models.Transcode.Id, args : String*) : String = {
    val cmd = ctlCmd ++ Seq("-i", id.toString) ++ args
    logger.debug(cmd.mkString(" "))
    Process(cmd).!!(procLogger(id.toString)).trim
  }

  /* we use database transactions here mainly to lock the transcode table */

  def start(asset : models.Asset, segment : Segment = Segment.full, options : String*)(implicit request : controllers.SiteRequest[_]) : Future[Boolean] =
    implicitly[Site.DB].inTransaction { implicit dbc =>
    models.Transcode.create(asset, request.identity, segment, options).flatMap { tc =>
      val pid = scala.util.control.Exception.catching(classOf[RuntimeException]).either {
	val r = ctl(tc.id, tc.args : _*)
	Maybe.toInt(r.trim).toRight("Unexpected transcode result: " + r)
      }.joinRight
      logger.debug("running " + tc.id + ": " + pid.merge.toString)
      tc.setStatus(pid)
    }
    }

  def stop(id : models.Asset.Id) : Future[Unit] =
    implicitly[Site.DB].inTransaction { implicit dbc =>
    models.Transcode.complete(id, None).flatMap(_.flatMapAsync { tc =>
      tc.setStatus(Left("aborted")).map {
	tc.pid.foreach((pid : Int) => ctl(tc.id, "-k", pid.toString))
      }
    })
    }

  def collect(id : models.Transcode.Id, pid : Int, res : Int, log : String) : Future[Boolean] =
    implicitly[Site.DB].inTransaction { implicit dbc =>
    models.Transcode.complete(id, Some(pid)).flatMap(_.flatMapAsync { tc =>
      // implicit val site = new LocalAuth(tc.owner, superuser = true)
      logger.debug("result " + tc.id + ": " + log)
      (for {
	_ <- tc.setStatus(Left(log))
	_ = if (res != 0) scala.sys.error("exit " + res)
	o = TemporaryFile(Upload.file(tc.id + ".mp4"))
	_ = ctl(asset.id, "-c", o.file.getAbsolutePath)
	r <- tc.fillOutput(o)
      } yield(r)).recoverWith { case e : Throwable =>
	logger.error("collecting " + id, e)
	tc.setStatus(Left(e.getMessage))
      }
    })
    }
}
