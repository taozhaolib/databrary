package ingest

import scala.concurrent.Future
import play.api.libs.json
import play.api.data.validation.ValidationError
import macros._
import macros.async._
import dbrary._
import site._

case class JsonException(errors: Seq[(json.JsPath, Seq[ValidationError])])
  extends IngestException(errors.mkString("\n"))

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

  private implicit object readOffset extends json.Reads[Option[Offset]] {
    def reads(j : json.JsValue) = j match {
      case json.JsNull => json.JsSuccess(None)
      case j => j.validate[Offset].map(Some(_))
    }
  }

  private implicit object readsSegment extends json.Reads[Segment] {
    private[this] def offset(o : json.JsValue) =
      readOffset.reads(o).map(_.fold[Segment](Range.full[Offset])(Range.singleton[Offset](_)))
    def reads(j : json.JsValue) = j match {
      case json.JsArray(Seq()) => json.JsSuccess(Range.full)
      case json.JsArray(Seq(o)) => offset(o)
      case json.JsArray(Seq(l, u)) => for {
          l <- readOffset.reads(l)
          u <- readOffset.reads(u)
        } yield (Range(l, u))
      case o => offset(o)
    }
  }

  private def popErr(msg : String)(implicit jc : JsContext) =
    Future.failed(PopulateException(msg + " in " + jc.path.toString))
}

final class Json(v : models.Volume, data : json.JsValue, overwrite : Boolean = false)(implicit site : Site) extends Ingest {
  import Json._
  validate(data)

  private[this] def write[A](current : Option[A], jc : JsContext)(change : A => Future[Boolean])(implicit read : json.Reads[A]) : Future[Unit] =
    jc.asOpt[A](read).fold(void) { v =>
      if (current.exists(_.equals(v)))
        void
      else if (current.isEmpty || overwrite)
        change(v).flatMap { r =>
          if (r) void else popErr("update failed")(jc)
        }
      else
        popErr("conflicting value: " + v + " <> " + current)(jc)
    }

  private[this] def record(implicit jc : JsContext) : Future[models.Record] = {
    val key = (jc \ "key").as[String]
    for {
      r <- models.Ingest.getRecord(v, key)
      r <- r.fold {
        for {
          r <- models.Record.create(v,
            category = (jc \ "category").as[Option[models.RecordCategory]])
          _ <- models.Ingest.setRecord(r, key)
        } yield (r)
      } { r => for {
          _ <- write(Some(r.category), jc \ "category")(_ => popErr("can't change record category"))
        } yield (r)
      }

      _ <- jc.children
        .filterNot(jc => Seq("key", "category", "position").contains(jc.key.as[String]))
        .foreachAsync { implicit jc =>
          val met = jc.key.as[models.Metric[_]]
          write(r.measures.datum(met), jc)(v => r.setMeasure(new models.Measure(met, v)))(readsValue)
        }
    } yield (r)
  }

  private[this] def container(implicit jc : JsContext) : Future[models.Container] = {
    val key = (jc \ "key").as[String]
    for {
      c <- models.Ingest.getContainer(v, key)
      c <- c.fold {
        for {
          c <- models.Container.create(v,
            top = (jc \ "top").asOpt[Boolean].getOrElse(false),
            name = (jc \ "name").asOpt[String],
            date = (jc \ "date").asOpt[Date])
          _ <- models.Ingest.setContainer(c, key)
        } yield (c)
      } { c => for {
          _ <- write(Some(c.top), jc \ "top")(_ => popErr("can't change container top"))
          _ <- write(c.name, jc \ "name")(x => c.change(name = Some(Some(x))))
          _ <- write(c.date, jc \ "date")(x => c.change(date = Some(Some(x))))
        } yield (c)
      }

      _ <- write(Maybe(c.consent).opt, jc \ "consent")(c.setConsent(_))

      _ <- (jc \ "records").children.foreachAsync { implicit jc =>
        for {
          r <- record
          seg = (jc \ "position").as[Segment]
          _ <- models.SlotRecord.move(r, c, dst = seg)
        } yield ()
      }
    } yield (c)
  }

  def run() : Future[Seq[models.Container]] = {
    implicit val jc = JsContext(json.__, data)
    for {
      /* just to make sure it's the right volume: */
      _ <- write(Some(v.name), jc \ "name")(x => /* v.change(name = Some(x)) */
          popErr("refusing to overwrite mismatching volume name"))
      /* We don't actually ingest volume-level metadata through this interface:
      _ <- write(v.body, jc \ "body")(x => v.change(body = Some(Some(x))))
      _ <- write(v.alias, jc \ "alias")(x => v.change(alias = Some(Some(x))))
      cite <- v.citation
      ...
      */
     
      c <- (jc \ "containers").children.mapAsync(container(_))
    } yield (c)
  }
}
