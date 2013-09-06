package models

import java.sql.Date
import play.api.libs.Files.TemporaryFile
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

/** File formats for assets.
  * id should actually be a ShortId but it's just simpler to have Ints everywhere. */
sealed class AssetFormat private[models] (val id : AssetFormat.Id, val mimetype : String, val extension : Option[String], val name : String) extends TableRowId[AssetFormat] {
  /** mimetype split into its two components at the slash */
  def mimeSubTypes = {
    val slash = mimetype.indexOf('/')
    if (slash == -1)
      (mimetype, "")
    else
      (mimetype.substring(0, slash), mimetype.substring(slash+1))
  }
}

/** Specialization of [[AssetFormat]] for timeseries files stored in special internal formats.
  * Note that some non-TimeseriesFormat AssetFormats may represent timeseries data, but they are not interpreted as such. */
sealed abstract class TimeseriesFormat private[models] (override val id : TimeseriesFormat.Id, mimetype : String, extension : Option[String], name : String) extends AssetFormat(id, mimetype, extension, name) {
  /** The type of data produced when this timeseries is sampled at a single point.
    * For some data this may be a single number, in which case this may need to be extended. */
  val sampleFormat : AssetFormat
}

/** Interface for non-timeseries file formats. */
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

  /** Lookup a format by its mimetime. */
  def getMimetype(mimetype : String)(implicit db : Site.DB) : Option[AssetFormat] =
    SELECT("WHERE mimetype = {mimetype}").
      on('mimetype -> mimetype).singleOpt(row)
  /** Get a list of all file formats in the database */
  def getAll(implicit db : Site.DB) : Seq[AssetFormat] =
    SELECT("ORDER BY format.id").list(row)

  private[models] final val IMAGE : Id = asId(-1)
  /** File type for internal image data (jpeg).
    * Images of this type may be produced and handled specially internally.
    */
  object Image extends AssetFormat(IMAGE, "image/jpeg", Some("jpg"), "JPEG")
}

/** Interface to special timeseries formats.
  * These formats are all hard-coded so do not rely on the database, although they do have a corresponding timeseries_format table.
  * Currently this only includes Video, but may be extended to audio or other timeseries data. */
object TimeseriesFormat extends HasId[TimeseriesFormat] {
  /** Retrieve a timeseries format by id. */
  def get(id : Id) = id match {
    case VIDEO => Some(Video)
    case _ => None
  }

  private[models] final val VIDEO : Id = asId(-2)
  /** The designated internal video format. */
  object Video extends TimeseriesFormat(VIDEO, "video/mp4", Some("mp4"), "Video") {
    val sampleFormat = AssetFormat.Image
  }
}


/** Abstract base for all assets: objects within the system backed by primary file storage.
  * Unlike containers, no user-specific permission checking is done when retrieving assets or their data, so additional link-based checking must be done before presenting these to users. */
sealed abstract class Asset protected (val id : Asset.Id) extends TableRowId[Asset] with BackedAsset with Annotated {
  /** Format of this asset. */
  def format : AssetFormat
  /** Data classification for the data in this asset. */
  def classification : Classification.Value
  /** Participant consent level granted for this asset, which may depend on specific [[AssetLink]]s of this asset. */
  val consent : Consent.Value

  /** List of all AssetLinks via which this asset is linked into containers. */
  def containers(all : Boolean = true)(implicit site : Site) : Seq[AssetLink] = AssetLink.getContainers(this, all)(site)

  private[models] final def annotatedLevel = "asset"
  private[models] final def annotatedId = id
}

/** Assets which are backed by files on disk.
  * Currently this includes all of them. */
sealed trait BackedAsset {
  /** The backing asset from which this data is taken, which may be itself or a containing asset. */
  def source : Asset
  /** The backing asset from which this data is taken, which may be itself or a containing asset. */
  def sourceId : Asset.Id = source.id
}

/** Refinement (implicitly of Asset) for objects representing timeseries data. */
sealed trait TimeseriesData extends BackedAsset {
  /** The range of times represented by this object.
    * Should be a valid, bounded range. */
  def segment : Range[Offset]
  /** Length of time represented by this object, which may be zero if it is a single sample. */
  def duration : Offset = segment.upperBound.flatMap(u => segment.lowerBound.map(u - _)).get
  def source : Timeseries
  override def sourceId : Timeseries.Id = source.id
}

/** Base for simple "opaque" file assets, uploaded, stored, and downloaded as individual files with no special processing. */
sealed class FileAsset protected[models] (override val id : FileAsset.Id, val format : AssetFormat, val classification : Classification.Value, val consent : Consent.Value = Consent.NONE) extends Asset(id) with TableRowId[FileAsset] with BackedAsset {
  def source = this
  override def sourceId = id
}

/** Base for special timeseries assets in a designated format.
  * These assets may be handled in their entirety as FileAssets, extracted from to produce Clips.
  * They are never created directly by users but through a conversion process on existing FileAssets. */
final class Timeseries private[models] (override val id : Timeseries.Id, override val format : TimeseriesFormat, classification : Classification.Value, override val duration : Offset, consent : Consent.Value) extends FileAsset(id, format, classification, consent) with TableRowId[Timeseries] with TimeseriesData {
  override def source = this
  def segment : Range[Offset] = Range[Offset](0, duration)(PGSegment)
}

/** Base for clips of Timeseries.
  * These represent a selected, contiguous range (segment) of time within a Timeseries.
  * @param excerpt if this clip was identified for possible public release. Actual permission checks should use [[classification]] instead.
  */
final class Clip private (override val id : Clip.Id, val source : Timeseries, val segment : Range[Offset], val excerpt : Boolean, val consent : Consent.Value) extends Asset(id) with TableRowId[Clip] with TimeseriesData {
  def format = if (segment.isSingleton) source.format else source.format.sampleFormat
  def classification = {
    val c = source.classification
    if (c == Classification.IDENTIFIED && excerpt)
      Classification.EXCERPT
    else
      c
  }
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

  /** Retrieve a single asset according to its type.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  def get(i : Id)(implicit db : Site.DB) : Option[Asset] =
    SELECT("WHERE asset.id = {id}").on('id -> i).singleOpt()

  /** Retrieve the set of assets to which the given annotation is attached. */
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
  
  /** Retrieve a single (non-timeseries) file asset.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[FileAsset] =
    SELECT("WHERE file.id = {id}").
      on('id -> i).singleOpt()

  /** Create a new file asset based from an uploaded file.
    * @param format the format of the file, taken as given
    * @param file a complete, uploaded file which will be moved into the appropriate storage location
    */
  def create(format : AssetFormat, classification : Classification.Value, file : TemporaryFile)(implicit site : Site) : FileAsset = {
    val id = Audit.add(table, SQLArgs('format -> format.id, 'classification -> classification), "id").single(scalar[Id])
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
  
  /** Retrieve a single timeseries asset.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Timeseries] =
    SELECT("WHERE timeseries.id = {id}").
      on('id -> i).singleOpt()
}

object Clip extends AssetView[Clip]("clip") {
  import PGSegment.{column => segmentColumn}
  private[this] def makeSource(source : Timeseries)(id : Id, segment : Range[Offset], excerpt : Boolean, consent : Option[Consent.Value]) = new Clip(id, source, segment, excerpt, consent.getOrElse(Consent.NONE))
  private[models] def make(source : Timeseries) = (makeSource(source) _).tupled
  private[models] val columns = Columns[
    Id,  Range[Offset], Boolean,  Option[Consent.Value]](
    'id, 'segment,      'excerpt, SelectAs("asset_consent(clip.id, clip.segment)", "clip_consent"))
  private[models] val row = (columns ~ Timeseries.row) map {
    case (clip ~ source) => make(source)(clip)
  }
  private[models] override val src = "clip JOIN " + Timeseries.src + " ON clip.source = timeseries.id"
  
  /** Retrieve a single clip.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id)(implicit db : Site.DB) : Option[Clip] =
    SELECT("WHERE excerpt.id = {id}").
      on('id -> i).singleOpt()
}

