package models

import scala.concurrent.{ExecutionContext,Future}
import play.api.Play
import play.api.libs.Files.TemporaryFile
import play.api.mvc.RequestHeader
import macros._
import macros.async._
import dbrary._
import dbrary.SQL._
import site._

sealed class Transcode(val owner : Party, val orig : FileAsset, val segment : Segment, val options : IndexedSeq[String])
  extends InVolume {
  def asset : Asset = orig
  val id : Transcode.Id = asset.id
  def process : Option[Int] = None
  def log : Option[String] = None
  final def volume = orig.volume
  final def origId = orig.id
  final def ownerId = owner.id
  final def offset = segment.lowerBound.getOrElse(Offset.ZERO)
  final def fake = id === orig.id
}

final class TranscodeJob private[models] (override val asset : Asset, owner : Party, orig : FileAsset, segment : Segment, options : IndexedSeq[String], override val process : Option[Int] = None, override val log : Option[String] = None)
  extends Transcode(owner, orig, segment, options) with TableRowId[Asset] {
  private implicit final def context : ExecutionContext = _root_.site.context.foreground

  private[this] def args(implicit request : RequestHeader) = Seq(
    "-f", store.FileAsset.file(orig).getAbsolutePath,
    "-r", new controllers.AssetApi.TranscodedForm(id)._action.absoluteURL(request.secure),
    "--") ++
    (segment.lowerBound : Iterable[Offset]).flatMap(s => Seq("-ss", s.toString)) ++
    (segment.upperBound : Iterable[Offset]).flatMap(t => Seq("-t", (t-offset).toString)) ++
    options

  private[this] def update(pid : Option[Int] = None, log : Option[String] = None, lock : Option[Int] = process) =
    lsql"UPDATE transcode SET process = $pid, log = NULLIF(COALESCE(log || E'\\n', '') || COALESCE($log, ''), '') WHERE asset = $id AND COALESCE(process, 0) = ${lock.getOrElse(0)}"
    .run.ensure

  def start(implicit request : RequestHeader) : Future[Int] = {
    val lock = Some(-1)
    update(pid = lock, lock = None)
    .flatMap { _ =>
      store.Transcode.start(id, args)
      .andThen {
        case scala.util.Success(pid) => update(pid = Some(pid), lock = lock)
        case scala.util.Failure(e : Exception) => update(log = Some(e.getMessage), lock = lock)
      }
    }
  }

  def stop() =
    process.mapAsync { pid =>
      update(log = Some("aborted"))
      .flatMap(_ => store.Transcode.stop(id, pid))
    }

  private def complete(file : TemporaryFile, sha1o : Option[Array[Byte]]) : Future[TimeseriesAsset] = {
    val tp = media.AV.probe(file.file)
    val duration = tp.duration.filter(_ => tp.isVideo)
      .getOrElse(scala.sys.error("transcode check failed: " + id))
    val sha1 = sha1o.getOrElse(store.SHA1(file.file))
    for {
      _ <- Audit.change("asset", SQLTerms('duration -> duration, 'sha1 -> sha1, 'size -> file.file.length), SQLTerms('id -> id))
      a = new TimeseriesAsset(id, asset.volume, asset.format.asInstanceOf[TimeseriesFormat], asset.release, duration, asset.name, sha1)
      _ = store.FileAsset.store(a, file)
      _ <- lsql"UPDATE slot_asset SET segment = segment(lower(segment), lower(segment) + $duration) WHERE asset = $id"
        .execute
    } yield a
  }

  def collect(res : Int, sha1 : Option[Array[Byte]], log : String) : Future[TimeseriesAsset] =
    (for {
      _ <- update(log = Some(log))
      file <- store.Transcode.collect(id, res, log)
      a <- complete(file, sha1)
    } yield a)
    .whenFailure { case e : Exception =>
      update(log = Some(e.getMessage), lock = None)
    }
}

object Transcode extends TableId[Asset]("transcode") {
  private val columns = Columns(
      SelectColumn[Segment]("segment")
    , SelectColumn[IndexedSeq[String]]("options")
    , SelectColumn[Option[Int]]("process")
    , SelectColumn[Option[String]]("log")
    )
  /* This does not check permissions as you might expect */
  private val row = columns.join(
      Authorization.rowParent() on "transcode.owner = party.id",
      Asset.columns on "transcode.asset = asset.id",
      Asset.columns fromAlias "orig" on "transcode.orig = orig.id",
      Volume.columns on "asset.volume = volume.id AND orig.volume = volume.id"
    ).map { case ((segment, options, process, log), owner, asset, orig, volume) =>
      val vol = volume(Permission.ADMIN, new LocalAuth(owner, superuser = true))
      new TranscodeJob(asset(vol), owner.identity, orig(vol).asInstanceOf[FileAsset], segment, options, process, log)
    }

  def get(id : Id)(implicit exc : ExecutionContext) : Future[Option[TranscodeJob]] =
    row.SELECT(lsql"WHERE transcode.asset = $id")
    .singleOpt

  def getActive(implicit exc : ExecutionContext) : Future[Seq[TranscodeJob]] =
    row.SELECT(lsql"WHERE asset.sha1 IS NULL")
    .list

  val defaultOptions = IndexedSeq("-vf", """pad=iw+mod(iw\,2):ih+mod(ih\,2)""")

  def create(orig : FileAsset, segment : Segment = Segment.full, options : IndexedSeq[String] = defaultOptions, duration : Option[Offset] = None)(implicit site : Site, siteDB : Site.DB, exc : ExecutionContext) : Future[TranscodeJob] =
    for {
      asset <- Asset.createPending(orig.volume, orig.format.isTranscodable.get, orig.release, orig.name, duration)
      tc = new TranscodeJob(asset, site.identity, orig, segment, options)
      _ <- INSERT('asset -> tc.id, 'owner -> tc.ownerId, 'orig -> tc.origId, 'segment -> tc.segment, 'options -> tc.options).execute
      _ <- lsql"UPDATE slot_asset SET asset = ${asset.id}, segment = segment(lower(segment) + ${tc.offset}, COALESCE(lower(segment) + ${segment.upperBound}, upper(segment))) WHERE asset = ${orig.id}"
        .execute
    } yield tc

  def apply(orig : FileAsset, segment : Segment = Segment.full, options : IndexedSeq[String] = defaultOptions)(implicit site : Site) : Transcode =
    new Transcode(site.identity, orig, segment, options)
}
