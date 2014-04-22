package store

import java.io.File
import scala.concurrent.{Future,ExecutionContext,Await,duration}
import scala.sys.process.Process
import play.api.Play.current
import play.api.libs.Files.TemporaryFile
import dbrary._

object Transcode {
  private val context : ExecutionContext = play.api.libs.concurrent.Akka.system.dispatchers.lookup("transcode")
  private val logger = play.api.Logger("transcode")
  private val host = current.configuration.getString("transcode.host").flatMap(Maybe(_).opt)

  private def videoCmd(in : File, out : File) = {
    val level =
      if (logger.isTraceEnabled) "verbose"
      else if (logger.isDebugEnabled) "info"
      else if (logger.isInfoEnabled) "warning"
      else if (logger.isWarnEnabled) "error"
      else "fatal"
    /* When changing this, you must also change tools/hpc/transcode.pbs */
    Seq("ffmpeg", "-loglevel", level, "-threads", "1", "-i", in.getPath, "-vf", "pad='iw+mod(iw\\,2):ih+mod(ih\\,2)'", "-threads", "1", "-f", "mp4", "-c:v", "libx264", "-c:a", "libfdk_aac", "-y", out.getPath)
  }

  private def await[A](a : Future[A]) = Await.result(a, duration.Duration.Inf)

  def transcode(asset : models.Asset) : Unit = {
    val f = FileAsset.file(asset)
    val t = TemporaryFile(new File(f.getPath + ".tc"))
    Future {
      val logPrefix = asset.id.toString + ": "
      val log = scala.sys.process.ProcessLogger(
	s => logger.info(logPrefix + s), 
	s => logger.warn(logPrefix + s))
      if (asset.format.mimetype.startsWith("video/")) {
	val sp = media.AV.probe(f)
	val cmd = videoCmd(f, t.file)
	logger.debug(cmd.mkString(" "))
	val r = Process(cmd).!(log)
	if (r != 0)
	  throw new RuntimeException("failed: " + r)
	val tp = media.AV.probe(t.file)
	if (!tp.isVideo || (sp.duration - tp.duration).abs > Offset.ofSeconds(0.5))
	  throw new RuntimeException("check failed: " + sp.duration + "," + tp.duration)
	val a = await(models.Asset.create(asset.volume, models.AssetFormat.Video, asset.classification, tp.duration, asset.name, t))
	await(a.supersede(asset))
      } else
	throw new RuntimeException("unknown source type: " + asset.format.name)
    }.onFailure { case e : Throwable =>
      logger.error("transcoding " + asset.id, e)
    }
  }

  def stop(id : models.Asset.Id) : Unit = {
    SQL("SELECT process FROM transcode WHERE asset = ? AND process IS NOT NULL AND result IS NULL")
    .apply(id).singleOpt(SQLCols[Int])
    .onSuccess {
      case Some(pid) =>
    }
  }
}
