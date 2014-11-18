package models

import scala.concurrent.{ExecutionContext,Future}
import play.api.Play
import play.api.libs.Files.TemporaryFile
import play.api.mvc.RequestHeader
import macros._
import dbrary._
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
  final def start = segment.lowerBound.getOrElse(Offset.ZERO)
  final def fake = id === orig.id
}

final class TranscodeJob private[models] (override val asset : Asset, owner : Party, orig : FileAsset, segment : Segment, options : IndexedSeq[String], override val process : Option[Int] = None, override val log : Option[String] = None)
  extends Transcode(owner, orig, segment, options) with TableRowId[Asset] {
  def args(implicit request : RequestHeader) = Seq(
    "-f", store.FileAsset.file(orig).getAbsolutePath,
    "-r", new controllers.AssetApi.TranscodedForm(id)._action.absoluteURL(Play.isProd(Play.current)),
    "--") ++
    (segment.lowerBound : Iterable[Offset]).flatMap(s => Seq("-ss", s.toString)) ++
    (segment.upperBound : Iterable[Offset]).flatMap(t => Seq("-t", (t-start).toString)) ++
    options
  def setStatus(status : Either[String, Int])(implicit siteDB : Site.DB, exc : ExecutionContext) =
    SQL("UPDATE transcode SET process = ?, log = NULLIF(COALESCE(log || E'\\n', '') || COALESCE(?, ''), '') WHERE asset = ?")
    .apply(status.right.toOption, status.left.toOption, id).execute

  def complete(file : TemporaryFile, sha1 : Array[Byte])(implicit siteDB : Site.DB, exc : ExecutionContext) : Future[TimeseriesAsset] = {
    val tp = media.AV.probe(file.file)
    if (!tp.isVideo)
      scala.sys.error("transcode check failed: " + id)
    for {
      _ <- Audit.change("asset", SQLTerms('duration -> tp.duration, 'sha1 -> sha1, 'size -> file.file.length), SQLTerms('id -> id))
      a = new TimeseriesAsset(id, asset.volume, asset.format.asInstanceOf[TimeseriesFormat], asset.classification, tp.duration, asset.name, sha1)
      _ = store.FileAsset.store(a, file)
      _ <- SQL("UPDATE slot_asset SET segment = segment(lower(segment), lower(segment) + ?) WHERE asset = ?")
        .apply(a.duration, a.id).execute
    } yield (a)
  }
}

object Transcode extends TableId[Asset]("transcode") {
  /* This does not check permissions as you might expect */
  private val row = Columns(
      SelectColumn[Segment]("segment")
    , SelectColumn[IndexedSeq[String]]("options")
    , SelectColumn[Option[Int]]("process")
    , SelectColumn[Option[String]]("log")
    ).join(Party.row
      .leftJoin(Authorization.columns, "authorize_view.child = party.id AND authorize_view.parent = 0")
      .map { case (p, a) => Authorization.make(p)(a) },
      "transcode.owner = party.id")
    .join(Asset.columns, "transcode.asset = asset.id")
    .join(Asset.columns fromAlias "orig", "transcode.orig = orig.id")
    .join(Volume.columns, "asset.volume = volume.id AND orig.volume = volume.id")
    .map { case (((((segment, options, process, log), owner), asset), orig), volume) =>
      val vol = volume(Permission.ADMIN, new LocalAuth(owner, superuser = true))
      new TranscodeJob(asset(vol), owner.identity, orig(vol).asInstanceOf[FileAsset], segment, options, process, log)
    }
      
  def getJob(id : Id)(implicit siteDB : Site.DB, exc : ExecutionContext) : Future[Option[TranscodeJob]] =
    row.SELECT("""
      WHERE transcode.asset = ? AND asset.sha1 IS NULL
      FOR NO KEY UPDATE OF transcode""")
    .apply(id).singleOpt

  def get(id : Id)(implicit exc : ExecutionContext) : Future[Option[Transcode]] =
    row.SELECT("WHERE transcode.asset = ?")
    .apply(id).singleOpt

  def getActive(implicit site : Site, exc : ExecutionContext) : Future[Seq[Transcode]] =
    row.SELECT("WHERE asset.sha1 IS NULL")
    .apply().list

  def createJob(orig : FileAsset, segment : Segment, options : IndexedSeq[String])(implicit site : Site, siteDB : Site.DB, exc : ExecutionContext) : Future[TranscodeJob] =
    for {
      asset <- Asset.createPending(orig.volume, orig.format.isTranscodable.get, orig.classification, orig.name)
      tc = new TranscodeJob(asset, site.identity, orig, segment, options)
      _ <- INSERT('asset -> tc.id, 'owner -> tc.ownerId, 'orig -> tc.origId, 'segment -> tc.segment, 'options -> tc.options).execute
      _ <- SQL("UPDATE slot_asset SET asset = ?, segment = segment(lower(segment) + ?, COALESCE(lower(segment) + ?, upper(segment))) WHERE asset = ?")
        .apply(asset.id, tc.start, segment.upperBound, orig.id).execute
    } yield tc

  def apply(orig : FileAsset, segment : Segment = Segment.full, options : IndexedSeq[String] = IndexedSeq.empty[String])(implicit site : Site) =
    new Transcode(site.identity, orig, segment, options)
}
