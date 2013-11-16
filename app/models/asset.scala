package models

import scala.concurrent.Future
import scala.collection.concurrent
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
  AssetFormat.add(this)
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
  private val cache : concurrent.Map[Int, AssetFormat] = concurrent.TrieMap.empty[Int, AssetFormat]
  protected def add(f : AssetFormat) = cache.update(f.id.unId, f)

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

  private def gets(b : Boolean, a : Option[AssetFormat], f : => SQLRows[AssetFormat]) : Option[AssetFormat] =
    a.fold(Async.wait(f.singleOpt)) {
      case _ : TimeseriesFormat if !b => None
      case a => Some(a)
    }

  /** Lookup a format by its id.
    * @param ts include TimeseriesFormats. Unlike other lookups, this is enabled by default. */
  def get(id : Id, ts : Boolean = true) : Option[AssetFormat] =
    gets(ts, cache.get(id.unId),
      row.SELECT("WHERE id = ?").apply(id))
  private[models] def getTimeseries(id : TimeseriesFormat.Id) : Option[TimeseriesFormat] =
    cache.get(id.unId).flatMap(cast[TimeseriesFormat](_))
  /** Lookup a format by its mimetime.
    * @param ts include TimeseriesFormats. */
  def getMimetype(mimetype : String, ts : Boolean = false) : Option[AssetFormat] =
    gets(ts, cache collectFirst 
      { case (_, a) if a.mimetype.equals(mimetype) => a },
      row.SELECT("WHERE mimetype = ?").apply(mimetype))
  /** Lookup a format by its extension.
    * @param ts include TimeseriesFormats. */
  private def getExtension(extension : String, ts : Boolean = false) : Option[AssetFormat] =
    gets(ts, cache collectFirst 
      { case (_, a) if a.extension.fold(false)(_.equals(extension)) => a },
      row.SELECT("WHERE extension = ?").apply(extension))
  /** Get a list of all file formats in the database.
    * @param ts include TimeseriesFormats. */
  def getAll(ts : Boolean = false) : Iterable[AssetFormat] =
    // XX incomplete but possibly sufficient
    if (ts)
      cache.values
    else
      cache.values.filter(!_.isInstanceOf[TimeseriesFormat])

  def getFilename(filename : String, ts : Boolean = false) : Option[AssetFormat] =
    Maybe(filename.lastIndexOf('.')).opt.flatMap { i =>
      getExtension(filename.substring(i + 1).toLowerCase, ts)
    }
  def getFilePart(file : play.api.mvc.MultipartFormData.FilePart[_], ts : Boolean = false) : Option[AssetFormat] =
    file.contentType.flatMap(getMimetype(_, ts)) orElse
      getFilename(file.filename, ts)


  private[models] final val IMAGE : Id = asId(-700)
  /** File type for internal image data (jpeg).
    * Images of this type may be produced and handled specially internally.
    */
  final val Image = new AssetFormat(IMAGE, "image/jpeg", Some("jpg"), "JPEG")
}

/** Interface to special timeseries formats.
  * These formats are all hard-coded so do not rely on the database, although they do have a corresponding timeseries_format table.
  * Currently this only includes Video, but may be extended to audio or other timeseries data. */
object TimeseriesFormat extends HasId[TimeseriesFormat] {
  def get(id : Id) : Option[TimeseriesFormat] =
    AssetFormat.getTimeseries(id)

  private[models] final val VIDEO : Id = asId(-800)
  /** The designated internal video format. */
  final val Video = new TimeseriesFormat(VIDEO, "video/mp4", Some("mp4"), "Video") {
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

  def superseding = FileAsset.getSuperseding(id)
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
sealed class FileAsset protected[models] (override val id : FileAsset.Id, val format : AssetFormat, val classification : Classification.Value, val supersededId : Option[Asset.Id] = None) extends Asset(id) with TableRowId[FileAsset] with BackedAsset {
  def source = this
  override def sourceId = id
  def superseded : Option[Future[Asset]] = supersededId.map(Asset._get(_).single)
  protected[models] def supersede(f : FileAsset)(implicit site : Site) : Future[Boolean] =
    Audit.change("file", SQLTerms('superseded -> f.id), SQLTerms('id -> id)).execute
}

/** Base for special timeseries assets in a designated format.
  * These assets may be handled in their entirety as FileAssets, extracted from to produce Clips.
  * They are never created directly by users but through a conversion process on existing FileAssets. */
final class Timeseries private[models] (override val id : Timeseries.Id, override val format : TimeseriesFormat, classification : Classification.Value, supersededId : Option[Asset.Id] = None, override val duration : Offset) extends FileAsset(id, format, classification, supersededId) with TableRowId[Timeseries] with TimeseriesData {
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


object Asset extends TableId[Asset]("asset") {
  /* This is rather messy, but such is the nature of the dynamic query */
  private[models] val row = FileAsset.row
    .join(Clip.columns.?.from("asset LEFT JOIN clip USING (id)"), "file.id = asset.id OR file.id = clip.source")
    .map {
      case (asset, None) => asset
      case (asset : Timeseries, Some(clip)) => clip(asset)
    }
  private[models] val duration = "COALESCE(timeseries.duration, duration(clip.segment))"

  private[models] def _get(i : Id) : SQLRows[Asset] =
    row.SELECT("WHERE asset.id = ?").apply(i)
  /** Retrieve a single asset according to its type.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  def get(i : Id) : Future[Option[Asset]] =
    _get(i).singleOpt
}

object FileAsset extends TableId[FileAsset]("file") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[AssetFormat.Id]("format")
    , SelectColumn[Classification.Value]("classification")
    , SelectColumn[Option[Asset.Id]]("superseded")
    )
  private def make(id : FileAsset.Id, format : AssetFormat.Id, classification : Classification.Value, superseded : Option[Asset.Id]) =
    new FileAsset(id, AssetFormat.get(format, false).get, classification, superseded)
  private[models] val onlyRow =
    columns.map(make _) from "ONLY file"
  private[models] val row = 
    columns.leftJoin(Timeseries.column.column, "file.id = timeseries.id")
    .map {
      case (asset, None) =>
        (make _).tupled(asset)
      case ((id, format, classification, superseded), Some(duration)) =>
        Timeseries.make(id, format, classification, superseded, duration)
    }
  
  /** Retrieve a single (non-timeseries) file asset.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id, ts : Boolean = false) : Future[Option[FileAsset]] =
    (if (ts) row else onlyRow).SELECT("WHERE file.id = ?").apply(i).singleOpt

  def getSuperseding(i : Asset.Id) : Future[Seq[FileAsset]] =
    row.SELECT("WHERE file.superseded = ?").apply(i).list

  /** Create a new file asset from an uploaded file.
    * @param format the format of the file, taken as given
    * @param file a complete, uploaded file which will be moved into the appropriate storage location
    */
  def create(format : AssetFormat, classification : Classification.Value, file : TemporaryFile, superseding : Option[FileAsset] = None)(implicit site : Site) : Future[FileAsset] = {
    /* TODO transaction */
    Audit.add(table, SQLTerms('format -> format.id, 'classification -> classification), "id")
      .single(SQLCols[Id]).map { id =>
        store.FileAsset.store(id, file)
        new FileAsset(id, format, classification)
      }.flatMap { asset =>
        Async.map[FileAsset,Boolean](superseding, _.supersede(asset)).map { _ =>
          asset
        }
      }
  }
}

object Timeseries extends TableId[Timeseries]("timeseries") {
  private[models] val column = SelectColumn[Offset]("duration")
  private[models] def make(id : FileAsset.Id, format : AssetFormat.Id, classification : Classification.Value, superseded : Option[Asset.Id], duration : Offset) =
      new Timeseries(id.coerce[Timeseries], TimeseriesFormat.get(format.coerce[TimeseriesFormat]).get, classification, superseded, duration)
  private[models] val row =
    (FileAsset.columns ~+ column).map(make _).fromTable("timeseries")
  
  /** Retrieve a single timeseries asset.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id) : Future[Option[Timeseries]] =
    row.SELECT("WHERE timeseries.id = ?").apply(i).singleOpt

  /** Create a new timeseries asset from an uploaded file.
    * @param format the format of the file, taken as given
    * @param file a complete, uploaded file which will be moved into the appropriate storage location
    */
  def create(format : TimeseriesFormat, classification : Classification.Value, duration : Offset, file : TemporaryFile, superseding : Option[FileAsset] = None)(implicit site : Site) : Future[Timeseries] =
    /* TODO transaction */
    Audit.add(table, SQLTerms('format -> format.id, 'classification -> classification, 'duration -> duration), "id")
      .single(SQLCols[Id]).map { id =>
        store.FileAsset.store(id, file)
        new Timeseries(id, format, classification, None, duration)
      }.flatMap { asset =>
        Async.map[FileAsset,Boolean](superseding, _.supersede(asset)).map { _ =>
          asset
        }
      }
}

object Clip extends TableId[Clip]("clip") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[Range[Offset]]("segment")
    ).map { (id, segment) =>
      (source : Timeseries) => new Clip(id, source, segment)
    }
  private[models] val row = columns.join(Timeseries.row, "clip.source = timeseries.id") map {
    case (clip, source) => clip(source)
  }
  
  /** Retrieve a single clip.
    * This does not do any permissions checking, so an additional call to containers (or equivalent) will be necessary. */
  private[models] def get(i : Id) : Future[Option[Clip]] =
    row.SELECT("WHERE clip.id = ?").apply(i).singleOpt
}

