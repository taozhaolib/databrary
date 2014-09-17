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
  final def isTranscodable = isVideo // || isAudio
  
  def description = name

  final lazy val json = JsonRecord.flatten(id,
    Some('mimetype -> mimetype),
    extension.map('extension -> _),
    Some('name -> name),
    if (isTranscodable) Some('transcodable -> true) else None
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
  private val byId : scala.collection.immutable.Map[Id, AssetFormat] =
    list.map(f => f.id -> f).toMap

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


/** Assets which are backed by files on disk.
  * Currently this includes all of them. */
trait BackedAsset {
  /** The backing asset from which this data is taken, which may be itself or a containing asset. */
  def source : Asset
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
sealed class Asset protected (val id : Asset.Id, val volume : Volume, override val format : AssetFormat, private[this] var classification_ : Classification.Value, private[this] var name_ : Option[String], val sha1 : Array[Byte]) extends TableRowId[Asset] with BackedAsset with InVolume with SiteObject {
  /** Title or name of the asset as used in the container. */
  def name : Option[String] = name_
  def classification : Classification.Value = classification_

  def duration : Offset = Offset.ZERO
  def source = this
  override def sourceId = id

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

  def slot : Future[Option[SlotAsset]] = SlotAsset.getAsset(this)

  def link(c : Container, offset : Option[Offset] = None, duration : Offset = duration) : Future[SlotAsset] = {
    val seg = offset.fold[Segment](c.segment)(o => Segment(o, o + duration))
    for {
      _ <- Audit.changeOrAdd("slot_asset", SQLTerms('container -> c.id, 'segment -> seg), SQLTerms('asset -> id)).execute
    } yield (SlotAsset.make(this, seg, c, None))
  }
  def unlink : Future[Boolean] =
    Audit.remove("slot_asset", SQLTerms('asset -> id)).execute

  def isSuperseded : Future[Boolean] =
    SQL("SELECT asset FROM asset_revision WHERE orig = ?").apply(id).execute
  def supersede(old : Asset) : Future[Boolean] =
    SQL("SELECT asset_supersede(?, ?)").apply(old.id, id).execute

  def pageName = name.getOrElse("file")
  def pageParent = Some(volume)
  def pageURL = controllers.routes.AssetHtml.view(id)

  lazy val json : JsonRecord = JsonRecord.flatten(id,
    Some('format -> format.id),
    Some('classification -> classification),
    name.map('name -> _),
    cast[TimeseriesAsset](this).map('duration -> _.duration)
  )

  def json(options : JsonOptions.Options) : Future[JsonRecord] =
    JsonOptions(json, options,
      "slot" -> (opt => slot.map(_.fold[JsValue](JsNull)(_.slot.json.js))),
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

/** Special timeseries assets in a designated format.
  * These assets may be handled in their entirety as FileAssets, extracted from to produce Clips.
  * They are never created directly by users but through a conversion process on existing FileAssets. */
final class TimeseriesAsset private[models] (id : Asset.Id, volume : Volume, override val format : TimeseriesFormat, classification : Classification.Value, override val duration : Offset, name : Option[String], sha1 : Array[Byte]) extends Asset(id, volume, format, classification, name, sha1) with TimeseriesData {
  override def source = this
  def entire = true
  def section : Section = Segment(Offset.ZERO, duration)
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
    , SelectColumn[Array[Byte]]("sha1")
    ).map { (id, format, classification, duration, name, sha1) =>
      duration.fold(
        (volume : Volume) => new Asset(id, volume, AssetFormat.get(format).get, classification, name, sha1))(
        dur => (volume : Volume) => new TimeseriesAsset(id, volume, AssetFormat.getTimeseries(format).get, classification, dur, name, sha1))
    }

  private def rowVolume(volume : Selector[Volume]) : Selector[Asset] = columns
    .join(volume, "asset.volume = volume.id").map(tupleApply)
  private def rowVolume(volume : Volume) : Selector[Asset] =
    rowVolume(Volume.fixed(volume))
  private[models] def row(implicit site : Site) =
    rowVolume(Volume.row)

  def get(a : Asset.Id)(implicit site : Site) : Future[Option[Asset]] =
    row.SELECT("WHERE asset.id = ? AND", Volume.condition)
      .apply(a).singleOpt

  /** Get the list of older versions of this asset. */
  def getRevisions(a : Asset) : Future[Seq[Asset]] =
    rowVolume(a.volume)
    .SELECT("JOIN asset_revisions ON asset.id = orig WHERE asset_revisions.asset = ?")
    .apply(a.id).list

  /** Get a particular older version of this asset. */
  def getRevision(a : Asset, o : Id) : Future[Option[Asset]] =
    rowVolume(a.volume)
    .SELECT("JOIN asset_revisions ON asset.id = orig WHERE asset_revisions.asset = ? AND asset.id = ?")
    .apply(a.id, o).singleOpt

  def getAvatar(p : Party)(implicit site : Site) : Future[Option[Asset]] =
    rowVolume(Volume.Core)
    .SELECT("JOIN avatar ON asset.id = avatar.asset WHERE avatar.party = ?")
    .apply(p.id).singleOpt

  /** Create a new asset from an uploaded file.
    * @param format the format of the file, taken as given
    * @param file a complete, uploaded file which will be moved into the appropriate storage location
    */
  def create(volume : Volume, format : AssetFormat, classification : Classification.Value, name : Option[String], file : TemporaryFile)(implicit site : Site) : Future[Asset] = {
    implicit val defaultContext = context.process
    for {
      sha1 <- Future(store.SHA1(file.file))
      id <- Audit.add(table, SQLTerms('volume -> volume.id, 'format -> format.id, 'classification -> classification, 'name -> name, 'sha1 -> sha1), "id")
	.single(SQLCols[Id])
    } yield {
      val a = new Asset(id, volume, format, classification, name, sha1)
      store.FileAsset.store(a, file)
      a
    }
  }

  def create(volume : Volume, format : TimeseriesFormat, classification : Classification.Value, duration : Offset, name : Option[String], file : TemporaryFile)(implicit site : Site) : Future[Asset] = {
    implicit val defaultContext = context.process
    for {
      sha1 <- Future(store.SHA1(file.file))
      id <- Audit.add(table, SQLTerms('volume -> volume.id, 'format -> format.id, 'classification -> classification, 'duration -> duration, 'name -> name, 'sha1 -> sha1), "id")
	.single(SQLCols[Id])
    } yield {
      val a = new TimeseriesAsset(id, volume, format, classification, duration, name, sha1)
      store.FileAsset.store(a, file)
      a
    }
  }
}
