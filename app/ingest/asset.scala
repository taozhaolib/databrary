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

  def populate(volume : Volume, info : Asset.Info)(implicit request : controllers.SiteRequest[_], exc : ExecutionContext) : Future[models.Asset] =
    SQL("SELECT id FROM ingest.asset WHERE file = ?").apply(info.ingestPath).list(SQLCols[models.Asset.Id]).flatMap(_.toSeq match {
      case Nil =>
        /* for now copy and don't delete */
        val infile = store.TemporaryFileLinkOrCopy(info.file)
        val n = Maybe(name).opt
        for {
          asset <- info match {
            case Asset.TimeseriesInfo(_, fmt, duration, orig) =>
              for {
                o <- populate(volume, orig)
                a <- models.Asset.create(volume, fmt, classification, duration, n, infile)
                r <- SQL("INSERT INTO asset_revision VALUES (?, ?)").apply(o.id, a.id).execute
                if r
              } yield (a)
            case Asset.TranscodableFileInfo(_, fmt, _) if store.Transcode.enabled =>
              for {
                a <- models.Asset.create(volume, fmt, classification, n, infile)
                _ <- store.Transcode.start(a)
              } yield (a)
            case Asset.FileInfo(_, fmt) =>
              models.Asset.create(volume, fmt, classification, n, infile)
          }
          r <- SQL("INSERT INTO ingest.asset VALUES (?, ?)").apply(asset.id, info.ingestPath).execute
          if r
        } yield (asset)
      case Seq(iid) =>
        for {
          asset <- models.Asset.get(iid).map(_.get)
          _ <- check(asset.format == info.format,
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
        } yield (asset)
      case _ =>
        Future.failed(PopulateException("multiple imported assets for " + name + ": " + info.file.getPath))
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
    if (fmt.isTranscodable)
      Asset.TranscodableFileInfo(file, fmt, media.AV.probe(file).duration)
    else
      Asset.FileInfo(file, fmt)
  }
}
