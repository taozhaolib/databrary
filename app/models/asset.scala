package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.Files.TemporaryFile
import macros._
import dbrary._
import site._

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
  private[models] val row = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("mimetype")
    , SelectColumn[Option[String]]("extension")
    , SelectColumn[String]("name")
    ) map { (id, mimetype, extension, name) => id match {
      case IMAGE => Image
      case TimeseriesFormat.VIDEO => TimeseriesFormat.Video
      case _ => new AssetFormat(id, mimetype, extension, name)
    } } from "ONLY format"

  private def mts[A](b : Boolean, ts : => Option[A], a : => Future[Option[A]]) : Future[Option[A]] =
    if (b) Async.orElse(ts, a) else a

  /** Lookup a format by its id.
    * @param ts include TimeseriesFormats. Unlike other lookups, this is enabled by default. */
  def get(id : Id, ts : Boolean = true) : Future[Option[AssetFormat]] =
    mts(ts, TimeseriesFormat.get(id.coerce[TimeseriesFormat]),
      row.SELECT("WHERE id = ?").apply(id).singleOpt)
  /** Lookup a format by its mimetime.
    * @param ts include TimeseriesFormats. */
  def getMimetype(mimetype : String, ts : Boolean = false) : Future[Option[AssetFormat]] =
    mts(ts, TimeseriesFormat.getMimetype(mimetype),
      row.SELECT("WHERE mimetype = ?").apply(mimetype).singleOpt)
  /** Lookup a format by its extension.
    * @param ts include TimeseriesFormats. */
  private def getExtension(extension : String, ts : Boolean = false) : Future[Option[AssetFormat]] =
    mts(ts, TimeseriesFormat.getExtension(extension),
      row.SELECT("WHERE extension = ?").apply(extension).singleOpt)
  /** Get a list of all file formats in the database.
    * @param ts include TimeseriesFormats. */
  def getAll(ts : Boolean = false) : Future[Seq[AssetFormat]] =
    row.SELECT("ORDER BY format.id").apply().list.map {
      (if (ts) TimeseriesFormat.getAll else Nil) ++ _
    }

  def getFilename(filename : String, ts : Boolean = false) : Future[Option[AssetFormat]] =
    Async.flatMap[Int,AssetFormat](Maybe(filename.lastIndexOf('.')).opt, i =>
      getExtension(filename.substring(i + 1).toLowerCase, ts))
  def getFilePart(file : play.api.mvc.MultipartFormData.FilePart[_], ts : Boolean = false) : Future[Option[AssetFormat]] =
    Async.flatMap[String,AssetFormat](file.contentType, getMimetype(_, ts)).flatMap {
      Async.orElse(_, getFilename(file.filename, ts))
    }


  private[models] final val IMAGE : Id = asId(-700)
  /** File type for internal image data (jpeg).
    * Images of this type may be produced and handled specially internally.
    */
  object Image extends AssetFormat(IMAGE, "image/jpeg", Some("jpg"), "JPEG")
}

/** Interface to special timeseries formats.
  * These formats are all hard-coded so do not rely on the database, although they do have a corresponding timeseries_format table.
  * Currently this only includes Video, but may be extended to audio or other timeseries data. */
object TimeseriesFormat extends HasId[TimeseriesFormat] {
  def get(id : Id) = id match {
    case VIDEO => Some(Video)
    case _ => None
  }
  def getMimetype(mimetype : String) : Option[TimeseriesFormat] = mimetype match {
    case Video.mimetype => Some(Video)
    case _ => None
  }
  private[models] def getExtension(extension : String) : Option[TimeseriesFormat] = Some(extension) match {
    case Video.extension => Some(Video)
    case _ => None
  }
  def getAll : Seq[TimeseriesFormat] = Seq(Video)

  private[models] final val VIDEO : Id = asId(-800)
  /** The designated internal video format. */
  object Video extends TimeseriesFormat(VIDEO, "video/mp4", Some("mp4"), "Video") {
    val sampleFormat = AssetFormat.Image
  }
}


/** Abstract base for all assets: objects within the system backed by primary file storage.
  * Unlike containers, no user-specific permission checking is done when retrieving assets or their data, so additional link-based checking must be done before presenting these to users. */
sealed abstract class Asset protected (val id : Asset.Id) extends TableRowId[Asset] with BackedAsset {
  /** Format of this asset. */
  def format : AssetFormat
  /** Data classification for the data in this asset. */
  def classification : Classification.Value

  /** ContainerAsset via which this asset is linked into a container. */
  def link(implicit site : Site) = ContainerAsset.get(this)
}

/** Assets which are backed by files on disk.
  * Currently this includes all of them. */
trait BackedAsset {
  /** The backing asset from which this data is taken, which may be itself or a containing asset. */
  def source : FileAsset
  /** The backing asset from which this data is taken, which may be itself or a containing asset. */
  def sourceId : FileAsset.Id = source.id
}

/** Refinement (implicitly of Asset) for objects representing timeseries data. */
trait TimeseriesData extends BackedAsset {
  /** The range of times represented by this object.
    * Should be a valid, finite, bounded range. */
  def segment : Range[Offset]
  def entire : Boolean
  /** Length of time represented by this object, which may be zero if it is a single sample. */
  def duration : Offset = segment.upperBound.flatMap(u => segment.lowerBound.map(u - _)).get
  def source : Timeseries
  override def sourceId : Timeseries.Id = source.id
}

/** Base for simple "opaque" file assets, uploaded, stored, and downloaded as individual files with no special processing. */
sealed class FileAsset protected[models] (override val id : FileAsset.Id, val format : AssetFormat, val classification : Classification.Value) extends Asset(id) with TableRowId[FileAsset] with BackedAsset {
  def source = this
  override def sourceId = id
}

/** Base for special timeseries assets in a designated format.
  * These assets may be handled in their entirety as FileAssets, extracted from to produce Clips.
  * They are never created directly by users but through a conversion process on existing FileAssets. */
final class Timeseries private[models] (override val id : Timeseries.Id, override val format : TimeseriesFormat, classification : Classification.Value, override val duration : Offset) extends FileAsset(id, format, classification) with TableRowId[Timeseries] with TimeseriesData {
  override def source = this
  def entire = true
  def segment : Range[Offset] = Range[Offset](0, duration)
}

/** Base for clips of Timeseries.
  * These represent a selected, contiguous range (segment) of time within a Timeseries.
  */
final class Clip private (override val id : Clip.Id, val source : Timeseries, val segment : Range[Offset]) extends Asset(id) with TableRowId[Clip] with TimeseriesData {
  def entire = false
  def format = if (segment.isSingleton) source.format.sampleFormat else source.format
  def classification = source.classification
}


private[models] sealed abstract class AssetView[R <: Asset with TableRowId[R]](table : String) extends TableId[R](table) {
  private[models] def get(i : Id) : Option[R]
}

object Asset extends AssetView[Asset]("asset") {
  /* This is rather messy, but such is the nature of the dynamic query */
  private[models] val row = 
    (FileAsset.columns.~+[Option[Offset]](SelectColumn("timeseries", "duration")) ~
     AssetFormat.row ~ Clip.columns.?) map {
      case (((id, classification, None), format), None) => new FileAsset(id, format, classification)
      case (((id, classification, Some(duration)), format : TimeseriesFormat), clip) => {
        val ts = new Timeseries(id.coerce[Timeseries], format, classification, duration)
        clip.fold[Asset](ts)((Clip.make(ts) _).tupled)
      }
    } from """asset
    LEFT JOIN clip USING (id)
         JOIN file ON file.id = asset.id OR file.id = clip.source
    LEFT JOIN timeseries ON timeseries.id = file.id
         JOIN format ON file.format = format.id"""
  private[models] val duration = "COALESCE(timeseries.duration, duration(clip.segment))"

  /** Retrieve a single asset according to its type.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  def get(i : Id) : Future[Option[Asset]] =
    row.SELECT("WHERE asset.id = ?").apply(i).singleOpt
}

object FileAsset extends AssetView[FileAsset]("file") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Classification.Value]("classification")
    )
  private[models] val row = columns.join(AssetFormat.row, "ONLY " + _ + " JOIN " + _ + " ON file.format = format.id") map {
    case ((id, classification), format) => new FileAsset(id, format, classification)
  }
  
  /** Retrieve a single (non-timeseries) file asset.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id) : Future[Option[FileAsset]] =
    row.SELECT("WHERE file.id = ?").apply(i).singleOpt

  /** Create a new file asset from an uploaded file.
    * @param format the format of the file, taken as given
    * @param file a complete, uploaded file which will be moved into the appropriate storage location
    */
  def create(format : AssetFormat, classification : Classification.Value, file : TemporaryFile)(implicit site : Site) : Future[FileAsset] =
    /* TODO transaction */
    Audit.add(table, SQLTerms('format -> format.id, 'classification -> classification), "id")
      .single(SQLCols[Id]).map { id =>
        store.FileAsset.store(id, file)
        new FileAsset(id, format, classification)
      }
}

object Timeseries extends AssetView[Timeseries]("timeseries") {
  private val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[TimeseriesFormat.Id]("format")
    , SelectColumn[Classification.Value]("classification")
    , SelectColumn[Offset]("duration")
    )
  private[models] val row = columns map {
    (id, format, classification, duration) =>
      new Timeseries(id, TimeseriesFormat.get(format).get, classification, duration)
  }
  
  /** Retrieve a single timeseries asset.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id) : Future[Option[Timeseries]] =
    row.SELECT("WHERE timeseries.id = ?").apply(i).singleOpt

  /** Create a new timeseries asset from an uploaded file.
    * @param format the format of the file, taken as given
    * @param file a complete, uploaded file which will be moved into the appropriate storage location
    */
  def create(format : TimeseriesFormat, classification : Classification.Value, duration : Offset, file : TemporaryFile)(implicit site : Site) : Future[Timeseries] =
    /* TODO transaction */
    Audit.add(table, SQLTerms('format -> format.id, 'classification -> classification, 'duration -> duration), "id")
      .single(SQLCols[Id]).map { id =>
        store.FileAsset.store(id, file)
        new Timeseries(id, format, classification, duration)
      }
}

object Clip extends AssetView[Clip]("clip") {
  private[models] def make(source : Timeseries)(id : Id, segment : Range[Offset]) = new Clip(id, source, segment)
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Range[Offset]]("segment")
    )
  private[models] val row = columns.join(Timeseries.row, "clip.source = timeseries.id") map {
    case (clip, source) => (make(source) _).tupled(clip)
  }
  
  /** Retrieve a single clip.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id) : Future[Option[Clip]] =
    row.SELECT("WHERE clip.id = ?").apply(i).singleOpt
}

