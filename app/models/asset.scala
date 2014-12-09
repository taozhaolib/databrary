package models

import scala.concurrent.{ExecutionContext,Future}
import scala.collection.concurrent
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{JsValue,JsNull}
import macros._
import dbrary._
import site._

/** File formats for assets.
  * id should actually be a ShortId but it's just simpler to have Ints everywhere. */
sealed class AssetFormat private[models] (val id : AssetFormat.Id, val mimetype : String, val extension : Option[String], val name : String) extends TableRowId[AssetFormat] {
  /** mimetype split into its two components at the slash */
  final def mimeSubTypes = {
    val slash = mimetype.indexOf('/')
    if (slash == -1)
      (mimetype, "")
    else
      (mimetype.substring(0, slash), mimetype.substring(slash+1))
  }
  final def isImage = mimetype.startsWith("image/")
  final def isVideo = mimetype.startsWith("video/")
  final def isAudio = mimetype.startsWith("audio/")
  final def isTranscodable : Option[AssetFormat] =
    if (!store.Transcode.enabled) None
    else if (isVideo) Some(AssetFormat.Video)
    else None
  
  def description = name

  def stripExtension(filename : String) : String =
    (for {
      ext <- extension
      i <- Maybe(filename.lastIndexOf('.')).opt
      if ext.equals(filename.substring(i + 1).toLowerCase)
    } yield (filename.substring(0, i))).getOrElse(filename)

  final lazy val json = JsonRecord.flatten(id,
    Some('mimetype -> mimetype),
    extension.map('extension -> _),
    Some('name -> name),
    if (description.equals(name)) None else Some('description -> description),
    isTranscodable.map('transcodable -> _.id)
  )
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
  private val row = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[String]("mimetype")
    , SelectColumn[Option[String]]("extension")
    , SelectColumn[String]("name")
    ) map { (id, mimetype, extension, name) =>
      new AssetFormat(id, mimetype, extension, name)
    } from "format"

  private[models] final val IMAGE : Id = asId(-700)
  private[models] final val VIDEO : Id = asId(-800)
  /** File type for internal image data (jpeg).
    * Images of this type may be produced and handled specially internally.
    */
  final val Image = new AssetFormat(IMAGE, "image/jpeg", Some("jpg"), "Image") {
    override def description = "JPEG Image"
  }
  /** The designated internal video format. */
  final val Video = new TimeseriesFormat(VIDEO, "video/mp4", Some("mp4"), "Video") {
    val sampleFormat = Image
    override def description = "MPEG-4 video"
  }

  private val list : Seq[AssetFormat] =
    Seq(Video, Image) ++ async.AWAIT {
      row.SELECT("WHERE id > 0 ORDER BY id").apply().list
    }
  private val byId : TableIdMap[AssetFormat] =
    TableIdMap(list : _*)

  /** Lookup a format by its id. */
  def get(id : Id) : Option[AssetFormat] =
    byId.get(id)
  private[models] def getTimeseries(id : Id) : Option[TimeseriesFormat] =
    byId.get(id).flatMap(cast[TimeseriesFormat](_))
  /** Lookup a format by its mimetime. */
  def getMimetype(mimetype : String) : Option[AssetFormat] =
    list.find(_.mimetype.equals(mimetype))
  /** Lookup a format by its extension.
    * @param ts include TimeseriesFormats. */
  private def getExtension(ext : String) : Option[AssetFormat] = {
    val extension = if (ext.equals("mpeg")) "mpg" else ext
    list.find(_.extension.exists(_.equals(extension)))
  }
  /** Get a list of all file formats in the database. */
  def getAll : Iterable[AssetFormat] = list

  def getFilename(filename : String) : Option[AssetFormat] =
    Maybe(filename.lastIndexOf('.')).opt.flatMap { i =>
      getExtension(filename.substring(i + 1).toLowerCase)
    }
  def getFilePart(file : play.api.mvc.MultipartFormData.FilePart[_]) : Option[AssetFormat] =
    file.contentType.flatMap(getMimetype(_)) orElse
      getFilename(file.filename)
}

sealed class Asset protected (val id : Asset.Id, val volume : Volume, val format : AssetFormat, private[this] var classification_ : Classification.Value, private[this] var name_ : Option[String], _duration : Option[Offset])
  extends TableRowId[Asset] with InVolume with SiteObject {
  /** Title or name of the asset as used in the container. */
  def name : Option[String] = name_
  def classification : Classification.Value = classification_
  def duration = _duration.getOrElse(Offset.ZERO)

  def creation : Future[(Option[Timestamp], Option[String])] =
    SQL("SELECT audit_time, name FROM audit.asset WHERE id = ? AND audit_action = 'add' ORDER BY audit_time DESC LIMIT 1")
      .apply(id).singleOpt(SQLCols[Option[Timestamp], Option[String]]).map(_.getOrElse((None, None)))

  /** Update the given values in the database and this object in-place. */
  def change(classification : Option[Classification.Value] = None, name : Option[Option[String]] = None) : Future[Boolean] = {
    Audit.change("asset", SQLTerms.flatten(classification.map('classification -> _), name.map('name -> _)), SQLTerms('id -> id)).execute
      .andThen { case scala.util.Success(true) =>
        classification.foreach(classification_ = _)
        name.foreach(name_ = _)
      }
  }

  def slot : Future[Option[AssetSlot]] = AssetSlot.getAssetFull(this)

  def link(c : Container, segment : Segment = Segment.full) : Future[SlotAsset] =
    Audit.changeOrAdd("slot_asset", SQLTerms('container -> c.id, 'segment -> segment), SQLTerms('asset -> id)).ensure
    .map(_ => SlotAsset.make(this, c, segment))
  def unlink : Future[Boolean] =
    Audit.remove("slot_asset", SQLTerms('asset -> id)).execute

  def isSuperseded : Future[Boolean] =
    SQL("SELECT id FROM asset_revision JOIN asset ON asset = id WHERE orig = ?").apply(id).execute
  def supersede(old : Asset) : Future[Boolean] =
    SQL("SELECT asset_supersede(?, ?)").immediately.apply(old.id, id).execute

  def pageName = name.getOrElse("file")
  def pageParent = Some(volume)
  def pageURL = controllers.routes.AssetHtml.view(id)

  def json : JsonRecord = JsonRecord.flatten(id,
    Some('format -> format.id),
    Some('classification -> classification),
    name.map('name -> _),
    if (this.isInstanceOf[BackedAsset]) None else Some('pending -> true)
  )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options,
      "slot" -> (opt => slot.map(_.fold[JsValue](JsNull)(sa => (sa.json - "asset").js))),
      "revisions" -> (opt => Asset.getRevisions(this).map(JsonArray.map(_.json))),
      "creation" -> (opt => if (checkPermission(Permission.EDIT))
        creation.map { case (date, name) => JsonObject.flatten(
          date.map('date -> _),
          name.map('name -> _))
          .js
        }
      else async(JsNull))
    )
}

/** Assets which are backed by files on disk.
  * Currently this includes all of them. */
trait BackedAsset {
  /** The backing asset from which this data is taken, which may be itself or a containing asset. */
  def source : FileAsset
  def sourceId : Asset.Id = source.id
  def format : AssetFormat = source.format
  def etag : String = "obj:" + sourceId
}

/** Refinement (implicitly of Asset) for objects representing timeseries data. */
trait TimeseriesData extends BackedAsset {
  def source : TimeseriesAsset
  /** The range of times represented by this object.
    * Should be a valid, finite, bounded range. */
  def section : Section
  def entire : Boolean
  /** Length of time represented by this object, which may be zero if it is a single sample. */
  def duration : Offset = section.upper - section.lower
  override def format : AssetFormat = if (section.isSingleton) source.format.sampleFormat else source.format
  override def etag : String =
    if (entire) super.etag
    else {
      val seg = section
      super.etag + ":" + seg.lower.millis +
        (if (seg.isSingleton) "" else "-" + seg.upper.millis)
    }
  def sample(offset : Offset) = new TimeseriesSample(this, offset)
}

/** File assets: objects within the system backed by primary file storage. */
sealed class FileAsset private[models] (id : Asset.Id, volume : Volume, override val format : AssetFormat, classification_ : Classification.Value, name_ : Option[String], val sha1 : Array[Byte])
  extends Asset(id, volume, format, classification_, name_, None) with BackedAsset {
  def source = this
  override def sourceId = id
}

/** Special timeseries assets in a designated format.
  * These assets may be handled in their entirety as FileAssets, extracted from to produce Clips.
  * They are never created directly by users but through a conversion process on existing FileAssets. */
final class TimeseriesAsset private[models] (id : Asset.Id, volume : Volume, override val format : TimeseriesFormat, classification : Classification.Value, override val duration : Offset, name : Option[String], sha1 : Array[Byte])
  extends FileAsset(id, volume, format, classification, name, sha1) with TimeseriesData {
  override def source = this
  def entire = true
  def section : Section = Segment(Offset.ZERO, duration)

  override def json : JsonRecord =
    super.json + ('duration -> duration)
}

final case class TimeseriesSample private[models] (val parent : TimeseriesData, val offset : Offset) extends TimeseriesData {
  def source = parent.source
  override def sourceId = parent.sourceId
  val section = {
    val seg = parent.section
    Range.singleton((seg.lower + offset).ensuring(seg @> _))
  }
  def entire = false
  override def duration = Offset.ZERO
  override def format = parent.source.format.sampleFormat
}

object Asset extends TableId[Asset]("asset") {
  private[models] val columns = Columns(
      SelectColumn[Id]("id")
    , SelectColumn[AssetFormat.Id]("format")
    , SelectColumn[Classification.Value]("classification")
    , SelectColumn[Option[Offset]]("duration")
    , SelectColumn[Option[String]]("name")
    , SelectColumn[Option[Array[Byte]]]("sha1")
    ).map { (id, format, classification, duration, name, sha1) =>
      sha1.fold(
        (volume : Volume) => new Asset(id, volume, AssetFormat.get(format).get, classification, name, duration))(
        sha1 => duration.fold(
          (volume : Volume) => new FileAsset(id, volume, AssetFormat.get(format).get, classification, name, sha1))(
          dur => (volume : Volume) => new TimeseriesAsset(id, volume, AssetFormat.getTimeseries(format).get, classification, dur, name, sha1)))
    }

  private def rowVolume(volume : Selector[Volume]) : Selector[Asset] = columns
    .join(volume, "asset.volume = volume.id").map(tupleApply)
  private[models] def rowVolume(volume : Volume) : Selector[Asset] =
    rowVolume(Volume.fixed(volume))
  private[models] def row(implicit site : Site) =
    rowVolume(Volume.row)
  private[models] def rowFileVolume(volume : Volume) : Selector[FileAsset] =
    rowVolume(Volume.fixed(volume)).map(_.asInstanceOf[FileAsset])

  def get(a : Asset.Id)(implicit site : Site) : Future[Option[Asset]] =
    row.SELECT("WHERE asset.id = ? AND", Volume.condition)
      .apply(a).singleOpt

  /** Get the list of older versions of this asset. */
  def getRevisions(a : Asset) : Future[Seq[FileAsset]] =
    rowFileVolume(a.volume)
    .SELECT("JOIN asset_revisions ON asset.id = orig WHERE asset_revisions.asset = ?")
    .apply(a.id).list

  /** Get a particular older version of this asset. */
  def getRevision(a : Asset, o : Id) : Future[Option[FileAsset]] =
    rowFileVolume(a.volume)
    .SELECT("JOIN asset_revisions ON asset.id = orig WHERE asset_revisions.asset = ? AND asset.id = ?")
    .apply(a.id, o).singleOpt

  private[models] def createPending(volume : Volume, format : AssetFormat, classification : Classification.Value, name : Option[String], duration : Option[Offset] = None)(implicit site : Site) : Future[Asset] =
    Audit.add(table, SQLTerms('volume -> volume.id, 'format -> format.id, 'classification -> classification, 'name -> name, 'duration -> duration), "id")
    .single(SQLCols[Id])
    .map(new Asset(_, volume, format, classification, name, duration))
}

object FileAsset extends TableId[Asset]("asset") {
  def getAvatar(p : Party)(implicit site : Site) : Future[Option[FileAsset]] =
    Asset.rowFileVolume(Volume.Core)
    .SELECT("JOIN avatar ON asset.id = avatar.asset WHERE avatar.party = ?")
    .apply(p.id).singleOpt

  /** Create a new asset from an uploaded file.
    * @param format the format of the file, taken as given
    * @param file a complete, uploaded file which will be moved into the appropriate storage location
    */
  def create(volume : Volume, format : AssetFormat, classification : Classification.Value, name : Option[String], file : TemporaryFile)(implicit site : Site) : Future[FileAsset] = {
    implicit val defaultContext = context.foreground
    for {
      sha1 <- Future(store.SHA1(file.file))
      id <- Audit.add(table, SQLTerms('volume -> volume.id, 'format -> format.id, 'classification -> classification, 'name -> name, 'sha1 -> sha1, 'size -> file.file.length), "id")
        .single(SQLCols[Id])
    } yield {
      val a = new FileAsset(id, volume, format, classification, name, sha1)
      store.FileAsset.store(a, file)
      a
    }
  }
}

object TimeseriesAsset extends TableId[Asset]("asset") {
  def create(volume : Volume, format : TimeseriesFormat, classification : Classification.Value, name : Option[String], file : TemporaryFile)(implicit site : Site) : Future[TimeseriesAsset] = {
    implicit val defaultContext = context.foreground
    for {
      probe <- Future(media.AV.probe(file.file))
      duration = probe.duration.get
      sha1 <- Future(store.SHA1(file.file))
      id <- Audit.add(table, SQLTerms('volume -> volume.id, 'format -> format.id, 'classification -> classification, 'duration -> duration, 'name -> name, 'sha1 -> sha1, 'size -> file.file.length), "id")
        .single(SQLCols[Id])
    } yield {
      val a = new TimeseriesAsset(id, volume, format, classification, duration, name, sha1)
      store.FileAsset.store(a, file)
      a
    }
  }
}
