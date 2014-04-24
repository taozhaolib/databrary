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
  private val enabled = current.configuration.getBoolean("transcode.enabled").getOrElse(false)
  private val host = current.configuration.getString("transcode.host").flatMap(Maybe(_).opt)
  private val dir = current.configuration.getString("transcode.dir").flatMap(Maybe(_).opt)

  private def procLogger(prefix : String) = {
    val pfx = if (prefix.nonEmpty) prefix + ": " else prefix
    scala.sys.process.ProcessLogger(
      s => logger.info(pfx + s), 
      s => logger.warn(pfx + s))
  }

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

  private def local(asset : models.Asset) : Unit = {
    val f = FileAsset.file(asset)
    val t = TemporaryFile(new File(f.getPath + ".tc"))
    Future {
      val log = procLogger(asset.id.toString)
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

  private lazy val ctl : File =
    current.resource("transctl.sh")
      .flatMap(urlFile(_))
      .getOrElse(throw new Exception("transctl.sh not found"))

  private def run(args : String*) : Unit =
    Process(ctl.getPath +: args).!(procLogger(

  def start(asset : models.Asset) : Unit =


  def stop(id : models.Asset.Id) : Unit = {
    SQL("SELECT process FROM transcode WHERE asset = ? AND process IS NOT NULL AND result IS NULL")
    .apply(id).singleOpt(SQLCols[Int])
    .onSuccess {
      case Some(pid) =>
    }
  }
}
