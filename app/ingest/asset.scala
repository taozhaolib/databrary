package ingest

import java.io.File
import scala.concurrent.{ExecutionContext,Future}
import macros._
import dbrary._
import models._
import site._

trait Asset {
  def name : String
  def classification : Classification.Value
  def info : Asset.Info
  def clip : Segment = Segment.full
  def options : IndexedSeq[String] = store.Transcode.defaultOptions
  def duration : Offset = (clip * Segment(Offset.ZERO, info.duration)).zip((l,u) => u-l).getOrElse(Offset.ZERO)
  var created : Boolean = false

  private[this] def populate(volume : Volume, info : Asset.Info)(implicit request : controllers.SiteRequest[_], exc : ExecutionContext) : Future[models.Asset] =
    models.Ingest.getAsset(volume, info.ingestPath).flatMap(_.fold {
        /* for now copy and don't delete */
        val infile = store.TemporaryFileLinkOrCopy(info.file)
        val n = Maybe(name).opt
        created = true
        info match {
          case Asset.TimeseriesInfo(_, fmt, _, orig) =>
            for {
              o <- populate(volume, orig)
              a <- models.TimeseriesAsset.create(volume, fmt, classification, n, infile)
              r <- models.Ingest.setAsset(a, info.ingestPath)
              if r
              r <- SQL("INSERT INTO asset_revision VALUES (?, ?)").apply(o.id, a.id).execute
              if r
            } yield a
          case Asset.TranscodableFileInfo(_, fmt, _) if store.Transcode.enabled =>
            for {
              o <- models.FileAsset.create(volume, fmt, classification, n, infile)
              r <- models.Ingest.setAsset(o, info.ingestPath)
              if r
              (t, run) <- store.Transcode.start(o, clip, options)
              r <- run
              if r
            } yield t.asset
          case _ =>
            for {
              a <- models.FileAsset.create(volume, info.format, classification, n, infile)
              r <- models.Ingest.setAsset(a, info.ingestPath)
              if r
            } yield a
        }
      } { asset =>
        for {
          _ <- check(asset.format === info.format,
            PopulateException("inconsistent format for asset " + name + ": " + info.format.name + " <> " + asset.format.name, asset))
          _ <- check(asset.classification == classification,
            PopulateException("inconsistent classification for asset " + name + ": " + classification + " <> " + asset.classification, asset))
          _ <- info match {
            case ts : Asset.TimeseriesInfo =>
              check(asset.asInstanceOf[TimeseriesAsset].duration.equals(ts.duration),
                PopulateException("inconsistent duration for asset " + name + ": " + ts.duration + " <> " + asset.asInstanceOf[TimeseriesAsset].duration, asset))
            case _ => async.void
          }
          _ <- if (asset.name.isEmpty) asset.change(name = Some(Maybe(name).opt))
            else check(asset.name.equals(Maybe(name).opt),
              PopulateException("inconsistent name for asset " + asset.name + ": " + name, asset))
        } yield asset
      })
  def populate(volume : Volume)(implicit request : controllers.SiteRequest[_], exc : ExecutionContext) : Future[models.Asset] =
    populate(volume, info)
}

private[ingest] object Asset {
  sealed abstract class Info {
    val file : File
    val format : AssetFormat
    final def ingestPath = store.Stage.path(file)
    def duration : Offset
  }
  final case class FileInfo(val file : File, val format : AssetFormat) extends Info {
    def duration : Offset = Offset.ZERO
  }
  final case class TranscodableFileInfo(val file : File, val format : AssetFormat, val duration : Offset) extends Info
  final case class TimeseriesInfo(val file : File, val format : TimeseriesFormat, val duration : Offset, original : Info) extends Info

  def fileInfo(file : File) : Info = {
    val fmt = AssetFormat.getFilename(file.getPath)
      .getOrElse(Parse.fail("no file format found for " + file))
    if (fmt.isTranscodable.nonEmpty)
      Asset.TranscodableFileInfo(file, fmt, media.AV.probe(file).duration)
    else
      Asset.FileInfo(file, fmt)
  }
}
