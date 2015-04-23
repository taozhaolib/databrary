package ingest

import scala.concurrent.Future
import play.api.libs.json
import play.api.data.validation.ValidationError
import macros._
import macros.async._
import dbrary._
import site._

case class JsonException(errors: Seq[(json.JsPath, Seq[ValidationError])])
  extends IngestException(errors.map { case (p, e) => p + ": " + e.mkString("; ") }.mkString("\n"))

private final case class JsContext(path : json.JsPath, data : json.JsValue) {
  def \(child : String) = JsContext(path \ child, data \ child)
  def apply(idx : Int) = JsContext(path(idx), data(idx))
  def as[A](implicit read : json.Reads[A]) : A = 
    read.reads(data).repath(path) match {
      case json.JsSuccess(v, _) => v
      case json.JsError(e) => throw JsonException(e)
    }
  def asOpt[A](implicit read : json.Reads[A]) : Option[A] = 
    if (data.isInstanceOf[json.JsUndefined])
      None
    else
      Some(as[A](read))
  def children : Seq[JsContext] = data match {
    case json.JsArray(l) => l.zipWithIndex.map { case (j, i) => JsContext(path(i), j) }
    case json.JsObject(l) => l.map { case (k, j) => JsContext(path \ k, j) }
    case _ => Nil
  }
  def key : json.JsValue = path.path.last match {
    case json.KeyPathNode(k) => json.JsString(k)
    case json.IdxPathNode(i) => json.JsNumber(i)
    case _ => json.JsNull
  }
}

private object Json {
  private[this] lazy val schema =
    com.github.fge.jsonschema.main.JsonSchemaFactory.byDefault.getJsonSchema(
      com.github.fge.jackson.JsonLoader.fromResource("/volume.json"))

  /* FIXME in play 2.4 this has been implemented properly: */
  private[this] def jsonToNode(j : json.JsValue) =
    new com.fasterxml.jackson.databind.ObjectMapper().readTree(json.Json.stringify(j))

  private def validate(j : json.JsValue) {
    val v = schema.validate(jsonToNode(j))
    if (!v.isSuccess)
      throw new IngestException(v.toString)
  }

  private implicit object readsCategory extends json.Reads[Option[models.RecordCategory]] {
    import models.RecordCategory
    private[this] def check(rc : Option[RecordCategory]) : json.JsResult[Option[RecordCategory]] =
      rc.fold[json.JsResult[Option[RecordCategory]]](
        json.JsError(Seq(json.JsPath() -> Seq(ValidationError("invalid record category")))))(
        c => json.JsSuccess(Some(c)))
    def reads(j : json.JsValue) = j match {
      case json.JsNull => json.JsSuccess(None)
      case json.JsString(s) => check(RecordCategory.getName(s))
      case json.JsNumber(i) => check(RecordCategory.get(RecordCategory.asId(i.toInt)))
      case _ => check(None)
    }
  }

  private implicit object readsMetric extends json.Reads[models.Metric[_]] {
    import models.Metric
    private[this] def check(rc : Option[Metric[_]]) : json.JsResult[Metric[_]] =
      rc.fold[json.JsResult[Metric[_]]](
        json.JsError(Seq(json.JsPath() -> Seq(ValidationError("invalid metric")))))(
        json.JsSuccess(_))
    def reads(j : json.JsValue) = check(j match {
      case json.JsString(s) => Metric.getName(s)
      case json.JsNumber(i) => Metric.get(Metric.asId(i.toInt))
      case _ => None
    })
  }

  private implicit object readsValue extends json.Reads[String] {
    def reads(j : json.JsValue) = j match {
      case json.JsString(s) => json.JsSuccess(s)
      case json.JsNumber(i) => json.JsSuccess(i.toString)
      case json.JsBoolean(b) => json.JsSuccess(b.toString)
      case json.JsNull => json.JsSuccess("")
      case _ => json.JsError(Seq(json.JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
  }

  private def readsMeasure[T](metric : models.Metric[T])(implicit read : json.Reads[T]) : json.Reads[models.Measure[T]] = new json.Reads[models.Measure[T]] {
    def reads(j : json.JsValue) = read.reads(j).map(new models.MeasureV[T](metric, _))
  }

  private implicit object readsOffset extends json.Reads[Option[Offset]] {
    def reads(j : json.JsValue) = j match {
      case json.JsNull => json.JsSuccess(None)
      case j => j.validate[Offset].map(Some(_))
    }
  }

  private implicit object readsSegment extends json.Reads[Segment] {
    private[this] def offset(o : json.JsValue) =
      readsOffset.reads(o).map(_.fold[Segment](Segment.full)(Segment.singleton(_)))
    def reads(j : json.JsValue) = j match {
      case _ : json.JsUndefined => json.JsSuccess(Segment.full)
      case json.JsArray(Seq()) => json.JsSuccess(Segment.full)
      case json.JsArray(Seq(o)) => offset(o)
      case json.JsArray(Seq(l, u)) => for {
          l <- readsOffset.reads(l)
          u <- readsOffset.reads(u)
        } yield (Range(l, u))
      case o => offset(o)
    }
  }

  private def popErr(target : site.SitePage, msg : String)(implicit jc : JsContext) =
    PopulateException(msg + " in " + jc.path.toString, target)
}

final class Json(v : models.Volume, data : json.JsValue, overwrite : Boolean = false)(implicit request : controllers.SiteRequest[_]) {
  private[this] implicit def context = site.context.background

  import Json._
  validate(data)
  private[this] val root = JsContext(json.__, data)
  private[this] val directory = store.Stage.file((root \ "directory").asOpt[String].getOrElse(""))
  if (!directory.isDirectory)
    throw new IngestException("stage directory not found: " + directory)

  private[this] def update[A](target : site.SitePage, current : Option[A], jc : JsContext, change : Option[A => Future[Boolean]] = None)(implicit read : json.Reads[A]) : Future[Option[A]] =
    jc.asOpt[A](read).fold[Future[Option[A]]](async(None)) { v =>
      if (current.exists(_.equals(v)))
        async(Some(v))
      else 
        change.filter(_ => current.isEmpty || overwrite).fold[Future[Option[A]]](
          Future.failed(popErr(target, "conflicting value: " + v + " <> " + current.get)(jc)))(
          _(v).map { r =>
            if (r) Some(v) else throw popErr(target, "update failed")(jc)
          })
    }
  private[this] def write[A](target : site.SitePage, current : Option[A], jc : JsContext)(change : A => Future[Boolean])(implicit read : json.Reads[A]) : Future[Option[A]] =
    update[A](target, current, jc, Some(change))(read)

  private[this] def record(implicit jc : JsContext) : Future[models.Record] = {
    val id = (jc \ "id").asOpt[models.Record.Id]
    val key = (jc \ "key").as[String]
    for {
      r <- models.Ingest.getRecord(v, key)
      r <- r.fold {
        for {
          r <- id.fold(models.Record.create(v,
            category = (jc \ "category").as[Option[models.RecordCategory]]))(i =>
            models.Record.get(i).map(_.filter(_.volume === v)
            .getOrElse(throw popErr(v, "record " + i + "/" + key + " not found"))))
          _ <- models.Ingest.setRecord(r, key)
        } yield (r)
      } { r =>
        if (!id.forall(_ === r.id))
          throw popErr(r, "record id mismatch")
        for {
          _ <- update(r, Some(r.category), jc \ "category")
        } yield (r)
      }

      _ <- jc.children
        .filterNot(jc => Seq("id", "key", "category", "position").contains(jc.key.as[String]))
        .foreachAsync { implicit jc =>
          val met = jc.key.as[models.Metric[_]]
          write(r, r.measures.datum(met), jc)(v => r.setMeasure(new models.Measure(met, v)))(readsValue)
        }
    } yield (r)
  }

  private[this] def asset(implicit jc : JsContext) : Future[models.Asset] = {
    val filename = (jc \ "file").as[String]
    val file = new java.io.File(directory, filename)
    if (!file.isFile)
      throw new IngestException("file not found: " + file)
    val fmt = models.AssetFormat.getFilename(filename)
      .getOrElse(throw new IngestException("no file format found for " + filename))
    val path = store.Stage.path(file)
    val clip = (jc \ "clip").asOpt[Segment].getOrElse(Segment.full)
    val options = (jc \ "options").asOpt[IndexedSeq[String]].getOrElse(models.Transcode.defaultOptions)

    models.Ingest.getAsset(v, path).flatMap(_.fold {
      /* for now copy and don't delete */
      val infile = store.TemporaryFileLinkOrCopy(file)
      for {
        a <- models.FileAsset.create(v, fmt, (jc \ "release").as[models.Release.Value], (jc \ "name").asOpt[String], infile)
        _ <- models.Ingest.setAsset(a, path)
      } yield a
    } {
      case a : models.FileAsset =>
        for {
          _ <- update(a, Some(a.release), jc \ "release")
          _ <- update(a, a.name, jc \ "name")
        } yield a
      case a =>
        Future.failed(popErr(a, "ingested asset incomplete"))
    }).flatMap { a =>
      if (a.format.isTranscodable.isEmpty) {
        if (!clip.isFull)
          Future.failed(popErr(a, "don't know how to clip"))
        else
          async(a)
      } else
        models.Ingest.getAssetClip(a, clip).flatMap(_.fold {
          val dur = (cast[models.TimeseriesAsset](a).map(_.duration)
            .orElse(media.AV.probe(store.FileAsset.file(a)).duration)
            .fold(Segment.full)(Segment(Offset.ZERO, _))
            * clip).zip((l, u) => u-l)
          for {
            t <- models.Transcode.create(a, clip, options, dur)
            _ <- t.start
          } yield t.asset
        } { a =>
          for {
            _ <- update(a, Some(a.release), jc \ "release")
            _ <- update(a, a.name, jc \ "name")
          } yield a
        })
    }
  }

  private[this] def container(implicit jc : JsContext) : Future[models.Container] = {
    val id = (jc \ "id").asOpt[models.Container.Id]
    val key = (jc \ "key").as[String]
    for {
      c <- models.Ingest.getContainer(v, key)
      c <- c.fold {
        for {
          c <- id.fold(models.Container.create(v,
            top = (jc \ "top").asOpt[Boolean].getOrElse(false),
            name = (jc \ "name").asOpt[String],
            date = (jc \ "date").asOpt[Date]))(i =>
            models.Container.get(i).map(_.filter(_.volume === v)
            .getOrElse(throw popErr(v, "container " + i + "/" + key + " not found"))))
          _ <- models.Ingest.setContainer(c, key)
        } yield (c)
      } { c => 
        if (!id.forall(_ === c.id))
          throw popErr(c, "container id mismatch")
        for {
          _ <- update(c, Some(c.top), jc \ "top")
          _ <- write(c, c.name, jc \ "name")(x => c.change(name = Some(Some(x))).map(_ => true))
          _ <- write(c, c.date, jc \ "date")(x => c.change(date = Some(Some(x))).map(_ => true))
        } yield (c)
      }

      // XXX this doesn't update the model
      _ <- write(c, Maybe(c.release).opt(), jc \ "release")(c.setRelease(_))

      _ <- (jc \ "records").children.foreachAsync { implicit jc =>
        for {
          r <- record
          seg = (jc \ "position").as[Segment]
          _ <- models.SlotRecord.move(r, c, dst = seg)
        } yield ()
      }

      _ <- (jc \ "assets").children.foldLeftAsync(Offset.ZERO) { (off, ajc) =>
        implicit val jc = ajc
        for {
          a <- asset(jc)
          l <- a.slot
          seg = l.map(_.segment)
          seg <- write(a, seg, jc \ "position")(s => a.link(new models.Slot { def context = c; def segment = s }).map(_ => true))(json.Reads {
            case json.JsString("auto") => json.JsSuccess(seg.getOrElse(Segment.singleton(off)))
            case j => readsSegment.reads(j)
          }.map(s => s.singleton.fold(s)(o => Segment(o, o + a.duration))))
        } yield (seg.flatMap(_.upperBound).getOrElse(off + a.duration) + Offset.SECOND)
      }
    } yield (c)
  }

  def run() : Future[Seq[models.Container]] = {
    implicit val jc = root
    for {
      /* just to make sure it's the right volume: */
      _ <- update(v, Some(v.name), jc \ "name")
      /* We don't actually ingest volume-level metadata through this interface:
      _ <- write(v, v.body, jc \ "body")(x => v.change(body = Some(Some(x))))
      _ <- write(v, v.alias, jc \ "alias")(x => v.change(alias = Some(Some(x))))
      cite <- v.citation
      ...
      */
     
      c <- (jc \ "containers").children.mapAsync(container(_))
    } yield (c)
  }
}
