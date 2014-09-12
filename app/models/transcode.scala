package models

import scala.concurrent.{ExecutionContext,Future}
import play.api.Play
import play.api.libs.Files.TemporaryFile
import play.api.mvc.RequestHeader
import macros._
import dbrary._
import site._

final class Transcode private[models] (val id : Transcode.Id, val owner : Access, val orig : Asset, val segment : Segment, val options : Seq[String], val process : Option[Int] = None, val log : Option[String] = None)
  extends TableRowId[Asset] with InVolume {
  def volume = orig.volume
  def ownerId = owner.identity.id
  def origId = orig.id
  def start = segment.lowerBound.getOrElse(Offset.ZERO)

  def args(implicit request : RequestHeader) = Seq(
    "-f", store.FileAsset.file(orig).getAbsolutePath,
    "-r", new controllers.AssetApi.TranscodedForm(orig.id)._action.absoluteURL(Play.isProd(Play.current)),
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
	.apply(id, start, start + a.duration)
    } yield (a)
  }
}

object Transcode extends TableId[Asset]("transcode") {
  private val columns = Columns(
      SelectColumn[Id]("asset")
    , SelectColumn[Segment]("segment")
    , SelectColumn[IndexedSeq[String]]("options")
    , SelectColumn[Option[Int]]("process")
    , SelectColumn[Option[String]]("log")
    ) map { (id, segment, options, process, log) =>
      (owner : Access, orig : Asset) =>
	new Transcode(id, owner, orig, segment, options, process, log)
    }
  private val row = columns
    .join(Party.row
      .leftJoin(Authorization.columns, "authorize_view.child = party.id AND authorize_view.parent = 0")
      .map { case (p, a) => Authorization.make(p)(a) },
      "transcode.owner = party.id")
    .join(Asset.columns, "transcode.orig = asset.id")
    .join(Volume.columns, "asset.volume = volume.id")
    .map { case (((t, o), a), v) =>
      t(o, a(v(Permission.ADMIN, new LocalAuth(o, superuser = true))))
    }

  def create(orig : Asset, segment : Segment, options : IndexedSeq[String])(implicit site : Site, siteDB : Site.DB, exc : ExecutionContext) : Future[Transcode] =
    SQL("INSERT INTO transcode (owner, orig, segment, options) VALUES (?, ?, ?, ?) RETURNING id")
    .apply(site.identity.id, orig.id, segment, options).single(SQLCols[Id])
    .map(new Transcode(_, site.access, orig, segment, options))

  def get(id : Id)(implicit siteDB : Site.DB, exc : ExecutionContext) : Future[Option[Transcode]] =
    row.SELECT("""
      WHERE transcode.asset = ?
        AND NOT EXISTS (SELECT asset.id FROM asset WHERE asset.id = transcode.asset)
      FOR NO KEY UPDATE OF transcode""")
    .apply(id).singleOpt
}
