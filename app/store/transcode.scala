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
    ctl.getPath +: (dir.toSeq.flatMap(d => Seq("-d", d.getPath)) ++ host.toSeq.flatMap(Seq("-h", _)))
  }

  private def ctl(aid : models.Asset.Id, args : String*) : String = {
    val cmd = ctlCmd ++ Seq("-a", aid.toString) ++ args
    logger.debug(cmd.mkString(" "))
    Process(cmd).!!(procLogger(aid.toString)).trim
  }

  private def setResult(aid : models.Asset.Id, pid : Option[Int], result : String) : Future[Boolean] =
    SQL("UPDATE transcode SET result = ?, process = NULL WHERE asset = ? AND", if (pid.isEmpty) "process IS NULL" else "process = ?")
    .apply(SQLArgs(result, aid) ++ SQLArgs.fromOption(pid)).execute

  def start(asset : models.Asset)(implicit request : controllers.SiteRequest[_]) : Future[Boolean] =
    (for {
      _ <- stop(asset.id)
      _ <- SQL("INSERT INTO transcode (asset, owner) VALUES (?, ?)")
	.apply(asset.id, request.identity.id).execute
      src = FileAsset.file(asset)
      pid = scala.util.control.Exception.catching(classOf[RuntimeException]) either {
	ctl(asset.id,
	  "-f", src.getAbsolutePath,
	  "-r", controllers.AssetApi.TranscodedForm(asset.id)._action.absoluteURL(play.api.Play.isProd))
      }
      _ = logger.debug("running " + asset.id + ": " + pid.merge.toString)
      r <- SQL("UPDATE transcode SET process = ?::integer, result = ? WHERE asset = ?")
	.apply(pid.right.toOption, pid.left.toOption.map(_.toString), asset.id).execute
    } yield (r)).recoverWith { case e : Throwable =>
      logger.error("starting " + asset.id, e)
      SQL("UPDATE transcode SET result = ? WHERE asset = ?")
	.apply(e.toString, asset.id).execute
    }

  def stop(id : models.Asset.Id) : Future[Unit] =
    for {
      pid <- SQL("DELETE FROM transcode WHERE asset = ? RETURNING process")
	.apply(id).singleOpt(SQLCols[Option[Int]])
      _ = pid.flatten.foreach((pid : Int) => ctl(id, "-k", pid.toString))
    } yield ()

  def collect(aid : models.Asset.Id, pid : Int, res : Int, log : String) : Future[Boolean] =
    if (res != 0)
      SQL("UPDATE transcode SET result = ? WHERE asset = ? AND process = ?")
	.apply("exit " + res + "\n" + log, aid, pid).execute
    else {
      SQL("DELETE FROM transcode WHERE asset = ? AND process = ? RETURNING owner")
      .apply(aid, pid).singleOpt(SQLCols[models.Party.Id])
      .flatMap(_.flatMapAsync(
	models.Authorization._get(_)
	.flatMap(_.flatMapAsync(a =>
	  models.Asset.get(aid)(new LocalAuth(a))))))
      .flatMap(_.foreachAsync({ asset =>
	val f = FileAsset.file(asset)
	val t = TemporaryFile(new File(f.getPath + ".mp4"))
	ctl(asset.id, "-c", t.file.getAbsolutePath)
	val sp = media.AV.probe(f)
	val tp = media.AV.probe(t.file)
	if (!tp.isVideo || (sp.duration - tp.duration).abs > Offset.ofSeconds(0.5))
	  scala.sys.error("check failed: " + sp.duration + "," + tp.duration)
	for {
	  a <- models.Asset.create(asset.volume, models.AssetFormat.Video, asset.classification, tp.duration, asset.name, t)
	  _ <- a.supersede(asset)
	  /* fixup slot_asset duration to reflect transcoding slop */
	  _ <- SQL("UPDATE slot_asset SET segment = segment(lower(segment), lower(segment) + ?) WHERE asset = ? AND duration(segment) = ? AND lower_inc(segment)")
	    .apply(tp.duration, a.id, sp.duration).execute
	} yield (())
      }, false))
      .recoverWith { case e : Throwable =>
	logger.error("collecting " + aid, e)
	SQL("INSERT INTO transcode (asset, result) VALUES (?, ?)")
	  .apply(aid, e.toString + "\n" + log).execute
      }
    }
}
