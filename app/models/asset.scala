package models

import java.sql.Date
import play.api.libs.Files.TemporaryFile
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

object Consent extends PGEnum("consent") {
  val PUBLIC, DEIDENTIFIED, EXCERPTS, SHARED, PRIVATE = Value
}

case class AssetFormat private[models] (id : AssetFormat.Id, mimetype : String, extension : Option[String], name : String, timeseries : Boolean) extends TableRowId[AssetFormat] {
  def mimeSubTypes = {
    val slash = mimetype.indexOf('/')
    if (slash == -1)
      (mimetype, "")
    else
      (mimetype.substring(0, slash), mimetype.substring(slash+1))
  }
}

private[models] sealed abstract class FormatView(table : String, timeseries : Boolean = false) extends TableId[AssetFormat](table) {
  private[models] val row = Columns[
    Id,  String,    Option[String], String](
    'id, 'mimetype, 'extension,     'name) map {
    (id, mimetype, extension, name) => new AssetFormat(id, mimetype, extension, name, timeseries)
  }

  def getMimetype(mimetype : String)(implicit db : Site.DB) : Option[AssetFormat] =
    SELECT("WHERE mimetype = {mimetype}").
      on('mimetype -> mimetype).singleOpt(row)
  def getAll(implicit db : Site.DB) : Seq[AssetFormat] =
    SELECT("ORDER BY asset.id").list(row)
}

object AssetFormat extends FormatView("format") {
  private[this] val columns = Columns[
    Id,  String,    Option[String], String, Long](
    'id, 'mimetype, 'extension,     'name,  'tableoid)
  private[this] def row(implicit db : Site.DB) = columns map {
    (id : Id, mimetype : String, extension : Option[String], name : String, tableoid : Long) => new AssetFormat(id, mimetype, extension, name, tableoid == TimeseriesFormat.tableOID)
  }

  override def getMimetype(mimetype : String)(implicit db : Site.DB) : Option[AssetFormat] =
    SELECT("WHERE mimetype = {mimetype}").
      on('mimetype -> mimetype).singleOpt(row(db))
  override def getAll(implicit db : Site.DB) : Seq[AssetFormat] =
    SELECT("ORDER BY asset.id").list(row(db))
}

object FileFormat extends FormatView("file_format", false) {
  final val Image = AssetFormat(asId(1), "image/jpeg", Some("jpg"), "JPEG", false)
}
object TimeseriesFormat extends FormatView("timeseries_format", true) {
  final val Video = AssetFormat(asId(2), "image/jpeg", Some("jpg"), "JPEG", true)
}


sealed abstract class Asset protected (val id : Asset.Id) extends TableRowId[Asset] {
  def consent : Consent.Value
  def format : AssetFormat
}

sealed class FileAsset protected (override val id : FileAsset.Id, val format : AssetFormat, val ownerId : Option[Study.Id], consent_ : Consent.Value, date_ : Option[Date]) extends Asset(id) with TableRowId[FileAsset] {
  private[this] var _consent = consent_
  def consent = _consent
  private[this] var _date = date_
  def date = _date

  def change(consent : Consent.Value = _consent, date : Option[Date] = _date)(implicit site : Site) : Unit = {
    if (date == _date && consent == _consent)
      return
    Audit.SQLon(AuditAction.change, "file", "SET consent = {consent}, date = {date} WHERE id = {id}")('consent -> consent, 'date -> date, 'id -> id).execute()(site.db)
    _consent = consent
    _date = date
  }
}

final class Timeseries private (override val id : Timeseries.Id, format : AssetFormat, ownerId : Option[Study.Id], consent_ : Consent.Value, date_ : Option[Date], val duration : Interval) extends FileAsset(id, format, ownerId, consent_, date_) with TableRowId[Timeseries] {
  /* this should be generalized for other privledged types: */
  def isVideo = format.mimetype.startsWith("video/")
}

final class Excerpt private (override val id : Excerpt.Id, val source : Timeseries, val offset : Interval, val duration : Option[Interval], public_ : Boolean) extends Asset(id) with TableRowId[Excerpt] {
  def sourceId = source.id
  private[this] var _public = public_
  def public = _public

  def change(public : Boolean = _public)(implicit site : Site) : Unit = {
    if (public == _public)
      return
    Audit.SQLon(AuditAction.change, "excerpt", "SET public = {public} WHERE id = {id}")('public -> public, 'id -> id).execute()(site.db)
    _public = public
  }

  def consent = {
    val c = source.consent
    if (c <= Consent.EXCERPTS && public)
      Consent.PUBLIC
    else
      c
  }
  def format = duration.fold(FileFormat.Image)(_ => source.format)
}


private[models] sealed abstract trait AssetView[R <: Asset with TableRowId[R]] extends TableView with HasId[R] {
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[R]
}

object Asset extends AssetView[Asset] {
  private[models] val table = "asset"
  private[models] override val src = "asset"
  type Row = String
  private[models] val row = Columns[String]('kind)
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Asset] =
    SELECT("WHERE id = {id}").on('id -> i).singleOpt() flatMap {
      case "file" => FileAsset.get(FileAsset.asId(i.unId))
      case "timeseries" => Timeseries.get(Timeseries.asId(i.unId))
      case "excerpt" => Excerpt.get(Excerpt.asId(i.unId))
    }

  /* This should really more appropriately be FileAsset, since it does not apply to Excerpts */
  def create(format : AssetFormat, owner : Study.Id, consent : Consent.Value, date : Option[Date], file : TemporaryFile)(implicit site : Site) : FileAsset = {
    val asset = if (format.timeseries)
        Timeseries.create(format, owner, consent, date, file)
      else
        FileAsset.create(format, owner, consent, date, file)
    store.FileAsset.store(asset.id, file)
    site.db.commit
    asset
  }
}

object FileAsset extends TableId[FileAsset]("file") with AssetView[FileAsset] {
  private[this] val columns = Columns[
    Id,  Option[Study.Id], Consent.Value, Option[Date]](
    'id, 'owner,           'consent,      'date)
  private[models] val row = (columns ~ FileFormat.row) map {
    case ((id, owner, consent, date) ~ format) => new FileAsset(id, format, owner, consent, date)
  }
  private[models] override val src = "ONLY file JOIN ONLY file_format ON file.format = file_format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[FileAsset] =
    SELECT("WHERE file.id = {id}").
      on('id -> i).singleOpt()

  private[models] def create(format : AssetFormat, owner : Study.Id, consent : Consent.Value, date : Option[Date], file : TemporaryFile)(implicit site : Site) : FileAsset = {
    val args = Anorm.Args('format -> format.ensuring(!_.timeseries).id, 'owner -> owner, 'consent -> consent, 'date -> date)
    val id = Audit.SQLon(AuditAction.add, table, Anorm.insertArgs(args), "id")(args : _*).single(scalar[Id])(site.db)
    new FileAsset(id, format, Some(owner), consent, date)
  }
}

object Timeseries extends TableId[Timeseries]("timeseries") with AssetView[Timeseries] {
  private[this] val columns = Columns[
    Id,  Option[Study.Id], Consent.Value, Option[Date], Interval](
    'id, 'owner,           'consent,      'date,        'duration)
  private[models] val row = (columns ~ TimeseriesFormat.row) map {
    case ((id, owner, consent, date, duration) ~ format) => new Timeseries(id, format, owner, consent, date, duration)
  }
  private[models] override val src = "timeseries JOIN timeseries_format ON timeseries.format = timeseries_format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Timeseries] =
    SELECT("WHERE timeseries.id = {id}").
      on('id -> i).singleOpt()

  private[models] def create(format : AssetFormat, owner : Study.Id, consent : Consent.Value, date : Option[Date], file : TemporaryFile)(implicit site : Site) : FileAsset = {
    val fmt = media.AV.probe(file.file)
    val duration = fmt.duration
    val args = Anorm.Args('format -> format.ensuring(_.timeseries).id, 'owner -> owner, 'consent -> consent, 'date -> date, 'duration -> duration)
    val id = Audit.SQLon(AuditAction.add, table, Anorm.insertArgs(args), "id")(args : _*).single(scalar[Id])(site.db)
    new Timeseries(id, format, Some(owner), consent, date, duration)
  }
}

object Excerpt extends TableId[Excerpt]("excerpt") with AssetView[Excerpt] {
  private[this] val columns = Columns[
    Id,  Interval, Option[Interval], Boolean](
    'id, 'offset,  'duration,        'public)
  private[models] val row = (columns ~ Timeseries.row) map {
    case ((id, offset, duration, public) ~ source) => new Excerpt(id, source, offset, duration, public)
  }
  private[models] override val src = "excerpt JOIN " + Timeseries.src + " ON excerpt.source = timeseries.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Excerpt] =
    SELECT("WHERE excerpt.id = {id}").
      on('id -> i).singleOpt()
}

