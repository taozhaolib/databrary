package models

import scala.concurrent.{ExecutionContext,Future}
import play.api.Play
import play.api.libs.Files.TemporaryFile
import play.api.mvc.RequestHeader
import macros._
import dbrary._
import site._

final class Transcode private[models] (val id : Transcode.Id, val owner : Access, val input : Asset, val segment : Segment, val options : Seq[String], val output : Option[Asset] = None, val process : Option[Int] = None, val log : Option[String] = None)
  extends TableRowId[Transcode] with InVolume {
  def volume = input.volume
  def ownerId = owner.identity.id
  def inputId = input.id
  def start = segment.lowerBound.getOrElse(Offset.ZERO)

  def args(implicit request : RequestHeader) = Seq(
    "-f", store.FileAsset.file(input).getAbsolutePath,
    "-r", new controllers.AssetApi.TranscodedForm(input.id)._action.absoluteURL(Play.isProd(Play.current)),
    "--") ++
    (segment.lowerBound : Iterable[Offset]).flatMap(s => Seq("-ss", s.toString)) ++
    (segment.upperBound : Iterable[Offset]).flatMap(t => Seq("-t", (t-start).toString)) ++
    options

  def setStatus(status : Either[String, Int])(implicit siteDB : Site.DB, exc : ExecutionContext) =
    SQL("UPDATE transcode SET process = ?, log = NULLIF(COALESCE(log || E'\\n', '') || COALESCE(?, ''), '') WHERE id = ?")
    .apply(status.right.toOption, status.left.toOption, id).execute

  def fillOutput(file : TemporaryFile)(implicit siteDB : Site.DB, exc : ExecutionContext) : Future[Asset] = {
    val tp = media.AV.probe(file.file)
    if (!tp.isVideo)
      scala.sys.error("transcode check failed: " + id)
    for {
      output <- models.Asset.create(volume, AssetFormat.Video, input.classification, tp.duration, input.name, file)
      _ <- SQL("INSERT INTO asset_revision (next, prev) VALUES (?, ?)")
	.apply(output.id, input.id).execute
      _ <- SQL("UPDATE transcode SET output = ? WHERE id = ?")
	.apply(output.id, id).execute
      _ <- SQL("UPDATE slot_asset SET asset = ?, segment = segment(lower(segment) + ?, lower(segment) + ?) WHERE asset = ?")
	.apply(output.id, start, start + output.duration)
    } yield (output)
  }
}

object Transcode extends TableId[Transcode]("transcode") {
  private val row = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Segment]("segment")
    , SelectColumn[IndexedSeq[String]]("options")
    , SelectColumn[Option[Int]]("process")
    , SelectColumn[Option[String]]("log")
    ) map { (id, segment, options, process, log) =>
      (owner : Access, input : Asset, output : Option[Asset]) =>
	new Transcode(id, owner, input, segment, options, output, process, log)
    }

  def create(input : Asset, segment : Segment, options : IndexedSeq[String])(implicit site : Site, siteDB : Site.DB, exc : ExecutionContext) : Future[Transcode] =
    SQL("INSERT INTO transcode (owner, input, segment, options) VALUES (?, ?, ?, ?) RETURNING id")
    .apply(site.identity.id, input.id, segment, options).single(SQLCols[Id])
    .map(new Transcode(_, site.access, input, segment, options))

  def get(id : Id, pid : Option[Int])(implicit siteDB : Site.DB, exc : ExecutionContext) : Future[Option[Transcode]] =
    row
    .join(Party.row
      .leftJoin(Authorization.columns, "authorize_view.child = party.id AND authorize_view.parent = 0")
      .map { case (p, a) => Authorization.make(p)(a) },
      "transcode.owner = party.id")
    .join(Asset.columns, "transcode.input = asset.id")
    .join(Volume.columns, "asset.volume = volume.id")
    .map { case (((t, o), a), v) =>
      t(o, a(v(Permission.ADMIN, new LocalAuth(o, superuser = true))), None)
    }.SELECT("WHERE transcode.id = ? AND transcode.process = COALESCE(?, transcode.process) AND transcode.output IS NULL FOR NO KEY UPDATE")
    .apply(id, pid)
    .singleOpt
}
