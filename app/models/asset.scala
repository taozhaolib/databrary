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
  * These formats are all hard-coded so do not rely on the database.
  * Currently this only includes Video, but may be extended to audio or other timeseries data.
  * Note that some non-TimeseriesFormat AssetFormats may represent timeseries data, but they are not interpreted as such. */
sealed abstract class TimeseriesFormat private[models] (id : AssetFormat.Id, mimetype : String, extension : Option[String], name : String) extends AssetFormat(id, mimetype, extension, name) {
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
    ) map { (id, mimetype, extension, name) =>
      new AssetFormat(id, mimetype, extension, name)
    } from "format"

  private def wait(f : => SQLRows[AssetFormat]) : Option[AssetFormat] =
    scala.concurrent.Await.result(f.singleOpt,
      scala.concurrent.duration.Duration(1, scala.concurrent.duration.MINUTES))

  /** Lookup a format by its id. */
  def get(id : Id) : Option[AssetFormat] =
    cache.get(id.unId)
      .orElse(wait(row.SELECT("WHERE id = ?").apply(id)))
  private[models] def getTimeseries(id : Id) : Option[TimeseriesFormat] =
    cache.get(id.unId).flatMap(cast[TimeseriesFormat](_))
  /** Lookup a format by its mimetime. */
  def getMimetype(mimetype : String) : Option[AssetFormat] =
    cache.collectFirst { case (_, a) if a.mimetype.equals(mimetype) => a }
      .orElse(wait(row.SELECT("WHERE mimetype = ?").apply(mimetype)))
  /** Lookup a format by its extension.
    * @param ts include TimeseriesFormats. */
  private def getExtension(ext : String) : Option[AssetFormat] = {
    val extension = if (ext.equals("mpeg")) "mpg" else ext
    cache.collectFirst { case (_, a) if a.extension.fold(false)(_.equals(extension)) => a }
      .orElse(wait(row.SELECT("WHERE extension = ?").apply(extension)))
  }
  /** Get a list of all file formats in the database. */
  def getAll : Iterable[AssetFormat] =
    // XXX incomplete but assymptotically correct
    cache.values

  def getFilename(filename : String) : Option[AssetFormat] =
    Maybe(filename.lastIndexOf('.')).opt.flatMap { i =>
      getExtension(filename.substring(i + 1).toLowerCase)
    }
  def getFilePart(file : play.api.mvc.MultipartFormData.FilePart[_]) : Option[AssetFormat] =
    file.contentType.flatMap(getMimetype(_)) orElse
      getFilename(file.filename)


  private[models] final val IMAGE : Id = asId(-700)
  private[models] final val VIDEO : Id = asId(-800)
  /** File type for internal image data (jpeg).
    * Images of this type may be produced and handled specially internally.
    */
  final val Image = new AssetFormat(IMAGE, "image/jpeg", Some("jpg"), "JPEG")
  /** The designated internal video format. */
  final val Video = new TimeseriesFormat(VIDEO, "video/mp4", Some("mp4"), "Video") {
    val sampleFormat = Image
  }
}


/** Assets which are backed by files on disk.
  * Currently this includes all of them. */
trait BackedAsset {
  /** The backing asset from which this data is taken, which may be itself or a containing asset. */
  def source : Asset
  /** The backing asset from which this data is taken, which may be itself or a containing asset. */
  def sourceId : Asset.Id = source.id
  def format : AssetFormat = source.format
  def etag : String
}

/** Refinement (implicitly of Asset) for objects representing timeseries data. */
trait TimeseriesData extends BackedAsset {
  def source : Timeseries
  /** The range of times represented by this object.
    * Should be a valid, finite, bounded range. */
  def segment : Range[Offset]
  def entire : Boolean
  /** Length of time represented by this object, which may be zero if it is a single sample. */
  def duration : Offset = segment.zip((l,u) => u-l).get
  override def format : AssetFormat = if (segment.isSingleton) source.format.sampleFormat else source.format
}

/** File assets: objects within the system backed by primary file storage. */
sealed class Asset protected (val id : Asset.Id, val volume : Volume, override val format : AssetFormat, classification_ : Classification.Value, name_ : String, body_ : Option[String]) extends TableRowId[Asset] with BackedAsset with InVolume with SiteObject {
  private[this] var _name = name_
  /** Title or name of the asset as used in the container. */
  def name : String = _name
  private[this] var _body = body_
  /** Optional description of this asset. */
  def body : Option[String] = _body
  private[this] var _classification = classification_
  def classification : Classification.Value = _classification

  def duration : Offset = 0
  def source = this
  override def sourceId = id
  def etag = "obj:" + id

  def creation : Future[Option[Timestamp]] =
    SQL("SELECT asset_creation(?)").apply(id).singleOpt(SQLCols[Timestamp])

  /** Update the given values in the database and this object in-place. */
  def change(classification : Classification.Value = _classification, name : String = _name, body : Option[String] = _body) : Future[Boolean] = {
    if (classification == _classification && name == _name && body == _body)
      return Async(true)
    Audit.change("asset", SQLTerms('classification -> classification, 'name -> name, 'body -> body), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        _classification = classification
        _name = name
        _body = body
      }
  }

  def slot : Future[Option[SlotAsset]] = SlotAsset.getAsset(this)

  def link(s : Slot) : Future[Boolean] =
    Audit.changeOrAdd("asset_slot", SQLTerms('slot -> s.id), SQLTerms('asset -> id)).execute
  def unlink : Future[Boolean] =
    Audit.remove("asset_slot", SQLTerms('asset -> id)).execute

  def pageName = name
  def pageParent = Some(volume)
  def pageURL = controllers.routes.Asset.view(volume.id, id)
  def pageActions = Seq(
    Action("view", controllers.routes.Asset.view(volumeId, id), Permission.VIEW),
    Action("edit", controllers.routes.Asset.edit(volumeId, id), Permission.EDIT),
    Action("remove", controllers.routes.Asset.remove(volumeId, id), Permission.CONTRIBUTE)
  )
}

/** Special timeseries assets in a designated format.
  * These assets may be handled in their entirety as FileAssets, extracted from to produce Clips.
  * They are never created directly by users but through a conversion process on existing FileAssets. */
final class Timeseries private[models] (id : Asset.Id, volume : Volume, override val format : TimeseriesFormat, classification : Classification.Value, override val duration : Offset, name : String, body : Option[String]) extends Asset(id, volume, format, classification, name, body) with TimeseriesData {
  override def source = this
  def entire = true
  def segment : Range[Offset] = Range[Offset](0, duration)
}

final case class TimeseriesSample private[models] (val parent : TimeseriesData, val offset : Offset) extends TimeseriesData {
  def source = parent.source
  override def sourceId = parent.sourceId
  val segment = {
    val seg = parent.segment
    Range.singleton((seg.lowerBound.get + offset).ensuring(seg @> _))
  }
  def entire = false
  override def duration = 0
  override def format = parent.source.format.sampleFormat
  def etag = parent.etag + ":sample:" + offset.millis
}


object Asset extends TableId[Asset]("asset") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[AssetFormat.Id]("format")
    , SelectColumn[Classification.Value]("classification")
    , SelectColumn[Option[Offset]]("duration")
    , SelectColumn[String]("name")
    , SelectColumn[Option[String]]("body")
    ).map { (id, format, classification, duration, name, body) =>
      duration.fold(
        (volume : Volume) => new Asset(id, volume, AssetFormat.get(format).get, classification, name, body))(
        dur => (volume : Volume) => new Timeseries(id, volume, AssetFormat.getTimeseries(format).get, classification, dur, name, body))
    }

  private def volumeRow(vol : Volume) =
    columns.map(_(vol))
  private def row(implicit site : Site) =
    columns.join(Volume.row, "asset.volume = volume.id")
      .map { case (asset, vol) => asset(vol) }


  def get(a : Asset.Id)(implicit site : Site) : Future[Option[Asset]] =
    row.SELECT("WHERE asset.id = ?").apply(a).singleOpt

  def getOlder(a : Asset, o : Id) : Future[Option[Asset]] =
    volumeRow(a.volume)
      .SELECT("JOIN asset_revision ON asset.id = prev WHERE next = ? AND asset.id = ? AND asset.volume = ?")
      .apply(a.id, o, a.volumeId).singleOpt

  /** Create a new asset from an uploaded file.
    * @param format the format of the file, taken as given
    * @param file a complete, uploaded file which will be moved into the appropriate storage location
    */
  def create(volume : Volume, format : AssetFormat, classification : Classification.Value, name : String, body : Option[String], file : TemporaryFile)(implicit site : Site) : Future[Asset] = {
    /* TODO transaction */
    Audit.add(table, SQLTerms('volume -> volume.id, 'format -> format.id, 'classification -> classification, 'name -> name, 'body -> body), "id")
      .single(SQLCols[Id]).map { id =>
        store.FileAsset.store(id, file)
        new Asset(id, volume, format, classification, name, body)
      }
  }

  def create(volume : Volume, format : TimeseriesFormat, classification : Classification.Value, duration : Offset, name : String, body : Option[String], file : TemporaryFile)(implicit site : Site) : Future[Asset] = {
    /* TODO transaction */
    Audit.add(table, SQLTerms('volume -> volume.id, 'format -> format.id, 'classification -> classification, 'duration -> duration, 'name -> name, 'body -> body), "id")
      .single(SQLCols[Id]).map { id =>
        store.FileAsset.store(id, file)
        new Timeseries(id, volume, format, classification, duration, name, body)
      }
  }
}
