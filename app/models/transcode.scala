package models

import scala.concurrent.{ExecutionContext,Future}
import play.api.Play
import play.api.libs.Files.TemporaryFile
import play.api.mvc.RequestHeader
import macros._
import dbrary._
import site._

sealed class Transcode(val owner : Party, val orig : Asset, val segment : Segment, val options : Seq[String])
  extends InVolume {
  def id : Asset.Id = orig.id
  def process : Option[Int] = None
  def log : Option[String] = None
  final def volume = orig.volume
  final def origId = orig.id
  final def ownerId = owner.id
  final def start = segment.lowerBound.getOrElse(Offset.ZERO)
  final def fake = id === orig.id
}

final class TranscodeJob private[models] (override val id : Transcode.Id, owner : Party, orig : Asset, segment : Segment, options : Seq[String], override val process : Option[Int] = None, override val log : Option[String] = None)
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

  def complete(file : TemporaryFile, sha1 : Array[Byte])(implicit siteDB : Site.DB, exc : ExecutionContext) : Future[Asset] = {
    val tp = media.AV.probe(file.file)
    if (!tp.isVideo)
      scala.sys.error("transcode check failed: " + id)
    for {
      _ <- Audit.add("asset", SQLTerms('id -> id, 'volume -> volumeId, 'format -> AssetFormat.Video.id, 'classification -> orig.classification, 'duration -> tp.duration, 'name -> orig.name, 'sha1 -> sha1))
      a = new TimeseriesAsset(id, volume, AssetFormat.Video, orig.classification, tp.duration, orig.name, sha1)
      _ = store.FileAsset.store(a, file)
      _ <- SQL("UPDATE slot_asset SET asset = ?, segment = segment(lower(segment) + ?, lower(segment) + ?) WHERE asset = ?")
	.apply(id, start, start + a.duration, orig.id)
    } yield (a)
  }
}

object Transcode extends TableId[Asset]("transcode") {
  /* This does not check permissions as you might expect */
  private val row = Columns(
      SelectColumn[Id]("asset")
    , SelectColumn[Segment]("segment")
    , SelectColumn[IndexedSeq[String]]("options")
    , SelectColumn[Option[Int]]("process")
    , SelectColumn[Option[String]]("log")
    ).join(Party.row
      .leftJoin(Authorization.columns, "authorize_view.child = party.id AND authorize_view.parent = 0")
      .map { case (p, a) => Authorization.make(p)(a) },
      "transcode.owner = party.id")
    .join(Asset.columns, "transcode.orig = asset.id")
    .join(Volume.columns, "asset.volume = volume.id")
    .map { case ((((id, segment, options, process, log), owner), orig), volume) =>
      new TranscodeJob(id, owner.identity, orig(volume(Permission.ADMIN, new LocalAuth(owner, superuser = true))), segment, options, process, log)
    }
      
  def getJob(id : Id)(implicit siteDB : Site.DB, exc : ExecutionContext) : Future[Option[TranscodeJob]] =
    row.SELECT("""
      WHERE transcode.asset = ?
        AND NOT EXISTS (SELECT asset.id FROM asset WHERE asset.id = transcode.asset)
      FOR NO KEY UPDATE OF transcode""")
    .apply(id).singleOpt

  def get(id : Id)(implicit exc : ExecutionContext) : Future[Option[Transcode]] =
    row.SELECT("WHERE transcode.asset = ?")
    .apply(id).singleOpt

  def getActive(implicit site : Site, exc : ExecutionContext) : Future[Seq[Transcode]] =
    row.SELECT("WHERE NOT EXISTS (SELECT asset.id FROM asset WHERE asset.id = transcode.asset)")
    .apply().list

  def createJob(orig : Asset, segment : Segment, options : IndexedSeq[String])(implicit site : Site, siteDB : Site.DB, exc : ExecutionContext) : Future[TranscodeJob] =
    SQL("INSERT INTO transcode (owner, orig, segment, options) VALUES (?, ?, ?, ?) RETURNING asset")
    .apply(site.identity.id, orig.id, segment, options).single(SQLCols[Id])
    .map(new TranscodeJob(_, site.identity, orig, segment, options))

  def apply(orig : Asset, segment : Segment = Segment.full, options : Seq[String] = Nil)(implicit site : Site) =
    new Transcode(site.identity, orig, segment, options)
}
