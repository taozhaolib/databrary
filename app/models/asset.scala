package models

import java.sql.Date
import play.api.libs.Files.TemporaryFile
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

object Classification extends PGEnum("classification") {
  val IDENTIFIED, EXCERPT, DEIDENTIFIED, ANALYSIS, PRODUCT, MATERIAL = Value
}

/* id should actually be a Short but who cares */
sealed class AssetFormat private[models] (val id : AssetFormat.Id, val mimetype : String, val extension : Option[String], val name : String) extends TableRowId[AssetFormat] {
  def mimeSubTypes = {
    val slash = mimetype.indexOf('/')
    if (slash == -1)
      (mimetype, "")
    else
      (mimetype.substring(0, slash), mimetype.substring(slash+1))
  }
}

sealed abstract class TimeseriesFormat private[models] (override val id : TimeseriesFormat.Id, mimetype : String, extension : Option[String], name : String) extends AssetFormat(id, mimetype, extension, name) {
  val sampleFormat : AssetFormat
}

object AssetFormat extends TableId[AssetFormat]("format") {
  private[models] val row = Columns[
    Id,  String,    Option[String], String](
    'id, 'mimetype, 'extension,     'name) map {
    (id, mimetype, extension, name) => id match {
      case IMAGE => Image
      case TimeseriesFormat.VIDEO => TimeseriesFormat.Video
      case _ => new AssetFormat(id, mimetype, extension, name)
    }
  }
  private[models] override val src = "ONLY format"

  def getMimetype(mimetype : String)(implicit db : Site.DB) : Option[AssetFormat] =
    SELECT("WHERE mimetype = {mimetype}").
      on('mimetype -> mimetype).singleOpt(row)
  def getAll(implicit db : Site.DB) : Seq[AssetFormat] =
    SELECT("ORDER BY format.id").list(row)

  private[models] final val IMAGE : Id = asId(-1)
  object Image extends AssetFormat(IMAGE, "image/jpeg", Some("jpg"), "JPEG")
}

object TimeseriesFormat extends HasId[TimeseriesFormat] {
  def get(id : Id) = id match {
    case VIDEO => Some(Video)
    case _ => None
  }

  private[models] final val VIDEO : Id = asId(-2)
  object Video extends TimeseriesFormat(VIDEO, "video/mp4", Some("mp4"), "Video") {
    val sampleFormat = AssetFormat.Image
  }
}


sealed abstract class Asset protected (val id : Asset.Id) extends TableRowId[Asset] with Annotated {
  def format : AssetFormat
  def classification : Classification.Value
  val consent : Consent.Value
  def containers(all : Boolean = true)(implicit site : Site) : Seq[AssetLink] = AssetLink.getContainers(this, all)(site)
  def fileId : FileAsset.Id
  def fileSegment : Option[Range[Offset]]
  private[models] final def annotatedLevel = "asset"
  private[models] final def annotatedId = id
}

sealed class FileAsset protected[models] (override val id : FileAsset.Id, val format : AssetFormat, val classification : Classification.Value, val consent : Consent.Value = Consent.NONE) extends Asset(id) with TableRowId[FileAsset] {
  def fileId = id
  def fileSegment = None
}

final class Timeseries private[models] (override val id : Timeseries.Id, override val format : TimeseriesFormat, classification : Classification.Value, val duration : Offset, consent : Consent.Value) extends FileAsset(id, format, classification, consent) with TableRowId[Timeseries] {
  def segment : Range[Offset] = Range[Offset](0, duration)(PGSegment)
}

final class Clip private (override val id : Clip.Id, val source : Timeseries, val segment : Range[Offset], val excerpt : Boolean, val consent : Consent.Value) extends Asset(id) with TableRowId[Clip] {
  def sourceId = source.id

  def format = if (segment.isSingleton) source.format else source.format.sampleFormat
  def classification = {
    val c = source.classification
    if (c == Classification.IDENTIFIED && excerpt)
      Classification.EXCERPT
    else
      c
  }
  def duration : Option[Offset] = segment.upperBound.flatMap(u => segment.lowerBound.map(u - _))
  def fileId = sourceId
  def fileSegment = Some(segment)
}


private[models] sealed abstract class AssetView[R <: Asset with TableRowId[R]](table : String) extends TableId[R](table) {
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[R]
}

object Asset extends AssetView[Asset]("asset") {
  /* This is rather messy, but such is the nature of the dynamic query */
  private[models] val row = 
    (FileAsset.columns.~+[Option[Offset]](SelectColumn("timeseries", "duration")) ~
     AssetFormat.row ~ Clip.columns.?) map {
      case (id, classification, consent, None) ~ format ~ None => new FileAsset(id, format, classification, consent.getOrElse(Consent.NONE))
      case (id, classification, consent, Some(duration)) ~ (format : TimeseriesFormat) ~ clip => {
        val ts = new Timeseries(id.coerce[Timeseries], format, classification, duration, consent.getOrElse(Consent.NONE))
        clip.fold(ts : Asset)(Clip.make(ts))
      }
    }
  private[models] override val src = """asset
    LEFT JOIN clip USING (id)
         JOIN file ON file.id = asset.id OR file.id = clip.source
    LEFT JOIN timeseries ON timeseries.id = file.id
         JOIN format ON file.format = format.id"""

  def get(i : Id)(implicit db : Site.DB) : Option[Asset] =
    SELECT("WHERE asset.id = {id}").on('id -> i).singleOpt()

  private[models] def getAnnotation(annotation : Annotation)(implicit db : Site.DB) : Seq[Asset] =
    SELECT("JOIN asset_annotation ON asset.id = asset WHERE annotation = {annotation}").
      on('annotation -> annotation.id).list()
}

object FileAsset extends AssetView[FileAsset]("file") {
  private[models] val columns = Columns[
    Id,  Classification.Value, Option[Consent.Value]](
    'id, 'classification,      SelectAs("asset_consent(file.id)", "file_consent"))
  private[models] val row = (columns ~ AssetFormat.row) map {
    case ((id, classification, consent) ~ format) => new FileAsset(id, format, classification, consent.getOrElse(Consent.NONE))
  }
  private[models] override val src = "ONLY file JOIN ONLY format ON file.format = format.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[FileAsset] =
    SELECT("WHERE file.id = {id}").
      on('id -> i).singleOpt()

  def create(format : AssetFormat, classification : Classification.Value, file : TemporaryFile)(implicit site : Site) : FileAsset = {
    val id = Audit.add(table, SQLArgs('format -> format, 'classification -> classification), "id").single(scalar[Id])(site.db)
    store.FileAsset.store(id, file)
    site.db.commit
    new FileAsset(id, format, classification)
  }
}

object Timeseries extends AssetView[Timeseries]("timeseries") {
  private[this] val columns = Columns[
    Id,  TimeseriesFormat.Id, Classification.Value, Offset,    Option[Consent.Value]](
    'id, 'format,             'classification,      'duration, SelectAs("asset_consent(timeseries.id)", "timeseries_consent"))
  private[models] val row = columns map {
    (id, format, classification, duration, consent) => new Timeseries(id, TimeseriesFormat.get(format).get, classification, duration, consent.getOrElse(Consent.NONE))
  }
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Timeseries] =
    SELECT("WHERE timeseries.id = {id}").
      on('id -> i).singleOpt()
}

object Clip extends AssetView[Clip]("clip") {
  implicit private val segmentColumn = PGSegment.column // why isn't this implicit found?
  private[this] def makeSource(source : Timeseries)(id : Id, segment : Range[Offset], excerpt : Boolean, consent : Option[Consent.Value]) = new Clip(id, source, segment, excerpt, consent.getOrElse(Consent.NONE))
  private[models] def make(source : Timeseries) = (makeSource(source) _).tupled
  private[models] val columns = Columns[
    Id,  Range[Offset], Boolean,  Option[Consent.Value]](
    'id, 'segment,      'excerpt, SelectAs("asset_consent(clip.id, clip.segment)", "clip_consent"))
  private[models] val row = (columns ~ Timeseries.row) map {
    case (clip ~ source) => make(source)(clip)
  }
  private[models] override val src = "clip JOIN " + Timeseries.src + " ON clip.source = timeseries.id"
  
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Clip] =
    SELECT("WHERE excerpt.id = {id}").
      on('id -> i).singleOpt()
}

