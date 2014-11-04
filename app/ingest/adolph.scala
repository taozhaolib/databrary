package ingest

import java.io.File
import scala.concurrent.Future
import macros._
import macros.async._
import dbrary._
import models._
import store.Stage
import site.Site

object Adolph extends Ingest {
  import Parse._

  private type ObjectParser[A] = Parser[A => A]
  private object ObjectParser {
    def apply[A](f : (A, String) => A) : ObjectParser[A] =
      Parser[A => A](s => f(_, s))
    def map[A,B](f : (A, B) => A, p : Parser[B]) : ObjectParser[A] =
      p.map(b => f(_, b))
    def option[A,B](f : (A, B) => A, p : Parser[B], nullif : String = "") : ObjectParser[A] =
      Parser[A => A] { s =>
        if (nullif != null && (s.isEmpty || s.equals(nullif)))
          identity[A]
        else
          f(_, p(s))
      }
  }

  private class CellParser[A](name : String, parse : ObjectParser[A])
    extends ColumnParser[A => A](name, parse)
  
  private def blankCellParser[A] : CellParser[A] =
    new CellParser[A]("<blank>", const[A => A](identity[A] _))

  private def parseCells[A](a : A, ps : Seq[CellParser[A]]) : ListParser[A] =
    ListParser[A] { l =>
      ps.foldLeft((a, l)) { (al, p) =>
        val (a, l) = al
        p.map(_(a))(l)
      }
    }

  private def measureParser[T](metric : Metric[T], p : Parser[T]) : Parser[MeasureV[T]] =
    Parser[MeasureV[T]](s => new MeasureV(metric, p(s)))

  private def dateMeasureParser(metric : Metric[Date]) =
    measureParser[Date](metric, date)

  private def measureMap(m : Measure[_]*) : Map[Metric.Id,Measure[_]] =
    Map(m.map(m => (m.metricId, m)) : _*)

  private abstract class Record(val category : RecordCategory) {
    protected val measures : Map[Metric.Id,Measure[_]]
    protected def idents : Iterable[Measure[_]]
    type Key
    def key : Key

    override def toString = category.name + ":" + idents.map(m => m.metric.name + ":" + m.datum).mkString(",")

    def getMeasure(m : Metric[_]) : Option[Measure[_]] =
      measures.get(m.id)
    protected final def addMeasure(m : Measure[_]) : Map[Metric.Id,Measure[_]] = {
      val i = m.metricId
      if (measures.contains(i))
        fail("duplicate measure for " + m.metric.name + " on " + this)
      measures.updated(i, m)
    }
    def withMeasure(m : Measure[_]) : Record

    def find(volume : Volume) : Future[Option[models.Record]] =
      for {
        l <- Record.findMeasures(volume, Some(category), idents.toSeq : _*)
        _ <- check(l.length <= 1,
          PopulateException("multiple matching records for " + this, volume))
      } yield (l.headOption)
    def populate(volume : Volume, current : Option[models.Record] = None) : Future[models.Record] =
      for {
        ro <- current orElseAsync find(volume)
        r <- ro getOrElseAsync Record.create(volume, Some(category))
        _ <- measures.values foreachAsync { m =>
          r.measures(m.metric).fold {
            r.setMeasure(m).flatMap(check(_, 
              PopulateException("failed to set measure for record " + this, r)))
          } { c =>
            check(c === m,
              PopulateException("inconsistent mesaures for record " + this + ": " + m + " <> " + c, r))
          }
        }
      } yield (r)
  }

  private final case class SingletonRecord(override val category : RecordCategory, measures : Map[Metric.Id,Measure[_]] = Map.empty) extends Record(category) {
    def idents = Nil
    type Key = Unit
    def key = ()
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }

  private final case class IdentRecord(override val category : RecordCategory, measures : Map[Metric.Id,Measure[_]] = Map.empty) extends Record(category) {
    def idents = getMeasure(Metric.Ident)
    type Key = String
    def key = getMeasure(Metric.Ident).fold(fail(category.name + " ident missing"))(_.datum)
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }

  private final case class TotalRecord(override val category : RecordCategory, measures : Map[Metric.Id,Measure[_]] = Map.empty) extends Record(category) {
    def idents = measures.values
    type Key = Map[Metric.Id,String]
    def key = measures.mapValues(_.datum)
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }

  private sealed class Category(name : String) {
    final val category = RecordCategory.getName(name).get
    final def id = category.id
  }

  private val metricInfo = Metric._getName[String]("info")
  private val metricDisability = Metric._getName[String]("disability")
  private val metricLanguage = Metric._getName[String]("language")
  private val metricCountry = Metric._getName[String]("country")
  private val metricState = Metric._getName[String]("state")

  private final case class Participant(measures : Map[Metric.Id,Measure[_]] = Map.empty)
    extends Record(Participant.category) {
    def idents = getMeasure(Metric.Ident) ++ getMeasure(metricInfo)
    type Key = (String, String)
    def key = (getMeasure(Metric.Ident).fold(fail("participant ID missing"))(_.datum), getMeasure(metricInfo).fold("")(_.datum))
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }
  private object Participant extends Category("participant")

  private type ParticipantMap = Map[(String, String), Participant]

  private object Participants {
    private final val empty = Participant()
    private def measure[T](m : Parser[MeasureV[T]], nullif : String = "") : Parser[Participant => Participant] =
      ObjectParser.option[Participant,MeasureV[T]](_.withMeasure(_), m, nullif)
    val parseId = measure(measureParser(Metric.Ident, trimmed), null)
    val parseSet = measure(measureParser(metricInfo, trimmed))
    private[Adolph] def parseHeader(name : String) : Parser[Participant => Participant] =
      name match {
        case "SUBJECT ID" => parseId
        case "DATASET" => parseSet
        case "BIRTH DATE" => measure(dateMeasureParser(Metric.Birthdate), null)
        case "BIRTH DATE (OPTIONAL)" => measure(dateMeasureParser(Metric.Birthdate))
        case "GENDER" => measure(Gender.measureParse)
        case "RACE" => measure(Race.measureParse)
        case "RACE (AS-IS)" => measure(measureParser(Race.metric, trimmed))
        case "ETHNICITY" => measure(Ethnicity.measureParse)
        case "TYPICAL DEVELOPMENT/DISABILITY" => measure(measureParser(metricDisability, trimmed), metricDisability.assumed.getOrElse(""))
        case "LANGUAGE" => measure(measureParser(metricLanguage, trimmed))
        case _ => fail("unknown header: " + name)
      }
    private def header : Parser[CellParser[Participant]] =
      Parser[CellParser[Participant]] {
        case "" => blankCellParser
        case s => new CellParser(s, parseHeader(s))
      }
    private def parseData(l : List[List[String]]) : ParticipantMap = l.zipWithIndex match {
      case h :: l =>
        val p = listAll(header).run(h)
        val line = parseCells(empty, p)
        l.foldLeft[ParticipantMap](Map.empty) { (m, l) =>
          val p = line.run(l)
          val i = p.key
          if (m.contains(i))
            throw ParseException("duplicate participant key: " + i, line = l._2)
          m.updated(i, p)
        }
      case Nil => Map.empty
    }
    final def parseCSV(f : File) =
      parseData(CSV.parseFile(f))
  }

  private final case class Exclusion(measures : Map[Metric.Id,Measure[_]] = Map.empty) extends Record(Exclusion.category) {
    def idents = getMeasure(Exclusion.Reason.metric)
    type Key = String
    def key = getMeasure(Exclusion.Reason.metric).fold(fail("exclusion reason missing"))(_.datum)
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }
  private object Exclusion extends Category("exclusion") {
    private object Reason extends MetricENUM("reason") {
      val DID_NOT_MEET_CRITERIA,
        PROCEDURAL_EXPERIMENTER_ERROR,
        FUSSY_TIRED_WITHDREW,
        OUTLIER = Value
    }

    def parse : Parser[Exclusion] =
      Reason.measureParse.map(m => Exclusion(measureMap(m)))
  }

  private object Context extends Category("context")
  private object Pilot extends Category("pilot")
  private object Condition extends Category("condition")
  private object Group extends Category("group")

  private object Setting extends MetricENUM("setting") {
    val LAB, HOME, MUSEUM, CLASSROOM, OUTDOOR, CLINIC = Value
  }

  private final case class Asset(name : String = "", path : File, classification : Classification.Value, offset : Offset) extends ingest.Asset {
    val info = {
      val file = Stage.file(path)
      if (!file.isFile)
        fail("file not found: " + path)
      Stage.findTranscoded(file) match {
        case Some((Some(origFile), file)) =>
          val probe = media.AV.probe(file)
          if (!probe.isVideo)
            fail("invalid file format for " + file + ": " + probe.format + " " + probe.streams.mkString(","))
          ingest.Asset.TimeseriesInfo(file, AssetFormat.Video, probe.duration, 
            ingest.Asset.fileInfo(origFile))
        case None =>
          ingest.Asset.fileInfo(file)
        case _ => fail("no original file: " + file)
      }
    }
  }

  private def recordMap(r : Record*) : Map[RecordCategory,Record] =
    Map(r.map(r => (r.category, r)) : _*)

  private final case class Session(name : String = "", date : Option[Date] = None, consent : Option[Consent.Value] = None, assetPath : Option[File] = None, assetOffset : Offset = Offset.ZERO, assets : Seq[Asset] = Nil, records : Map[RecordCategory,Record] = Map.empty) {
    def withName(n : String) =
      if (name.nonEmpty && !name.equals(n))
        fail("duplicate session name: " + n)
      else copy(name = n)
    def withDate(d : Date) =
      if (!date.forall(_.equals(d)))
        fail("duplicate session date: " + d)
      else copy(date = Some(d))
    def withConsent(c : Consent.Value) =
      if (!consent.forall(_.equals(c)))
        fail("duplicate session consent: " + c)
      else copy(consent = Some(c))
    def withParticipant(f : Participant => Participant) = {
      val i = Participant.category
      val p = records.get(i).fold(Participant())(_.asInstanceOf[Participant])
      copy(records = records.updated(i, f(p)))
    }
    def fillParticipant(pm : ParticipantMap) = {
      val i = Participant.category
      records.get(i).fold(fail("no participant")) { p =>
        copy(records = records.updated(i,
          pm.getOrElse(p.asInstanceOf[Participant].key, fail("participant not found: " + p))))
      }
    }
    def withLocation(f : Record => Record) = {
      val i = Context.category
      val p = records.getOrElse(i, TotalRecord(Context.category))
      copy(records = records.updated(i, f(p)))
    }
    def withRecord(r : Record) = {
      val i = r.category
      if (records.contains(i))
        fail("duplicate session record: " + r)
      copy(records = records.updated(i, r))
    }
    def withAssetPath(p : File) =
      copy(assetPath = Some(p))
    def withAssetOffset(o : Offset) =
      copy(assetOffset = o)
    def withAsset(a : Asset) =
      copy(assets = assets :+ a, assetOffset = assetOffset + a.info.duration + Offset.SECOND)

    def populate(volume : Volume)(implicit request : controllers.SiteRequest[_]) : Future[Container] =
      for {
        pr <- records(Participant.category).populate(volume)
        ps <- pr.slots
        ms = ps.filter(_.container.date.equals(date))
        _ <- check(ms.length <= 1,
          PopulateException("multiple existing sessions for participant", pr))
        c <- ms.headOption.fold {
          for {
            c <- Container.create(volume, name = Maybe(name).opt, date = date)
            _ <- consent.foreachAsync(c.setConsent(_))
            _ <- SlotRecord.move(pr, c, dst = c.segment)
          } yield (c)
        } { s =>
          for {
            _ <- check(s.volume === volume,
              PopulateException("existing session for " + pr + " with different volume", s))
            _ <- check(s.isFull,
              PopulateException("existing session for " + pr + " not full container", s))
            c = s.container
            _ <- check(c.name.equals(Maybe(name).opt),
              PopulateException("existing session for " + pr + " with different name", s))
            _ <- check(c.date.equals(date),
              PopulateException("existing session for " + pr + " with different date", s))
            _ <- if (c.consent == Consent.NONE) consent.foreachAsync(c.setConsent(_))
              else check(consent.exists(c.consent == _),
                PopulateException("existing session for " + pr + " with different consent", s))
          } yield (c)
        }
        _ <- assets foreachAsync { i =>
          for {
            a <- i.populate(volume)
            as <- a.slot
            _ <- as.fold[Future[Any]] {
              a.link(c, Segment(i.offset, i.offset + i.info.duration))
            } { as =>
              check(as.container === c,
                PopulateException("existing asset in different container", as))
            }
          } yield ()
        }
        cr <- c.fullRecords.map(_.groupBy(_.categoryId))
        _ <- (records - Participant.category).values foreachAsync { r =>
          val crs = cr.getOrElse(Some(r.category.id), Nil)
          for {
            _ <- check(crs.length <= 1,
              PopulateException("multiple existing records for category " + r.category, c))
            r <- r.populate(volume, crs.headOption)
            _ <- if (crs.isEmpty) SlotRecord.move(r, c, dst = c.segment) else async(false)
          } yield ()
        }
      } yield (c)
  }

  private object Sessions {
    private final val empty = Session()
    private def participant(p : Parser[Participant => Participant]) =
      ObjectParser.map[Session, Participant => Participant](_.withParticipant(_), p)
    private def location[T](m : Parser[MeasureV[T]], nullif : String = "") =
      ObjectParser.option[Session, Record => Record](_.withLocation(_),
        ObjectParser.map[Record, MeasureV[T]](_.withMeasure(_), m), nullif)
    private def record(c : RecordCategory, m : Parser[String], nullif : String = "") =
      ObjectParser.option[Session, Record](_.withRecord(_),
        m.map(v => IdentRecord(c, measureMap(new MeasureV[String](Metric.Ident, v)))))
    private val fileRegex = """FILE(?: \(([a-zA-Z]*)\))?(?:: (.*))?""".r
    private def parseHeader(name : String) : Parser[Session => Session] =
      name match {
        case "TEST DATE" => ObjectParser.map[Session, Date](_.withDate(_), date)
        case "SESSION ID" => ObjectParser.option[Session, String](_.withName(_), trimmed)
        case "RELEASE LEVEL" => ObjectParser.option[Session, Consent.Value](_.withConsent(_), consent)
        case "PILOT" => ObjectParser.option[Session, Unit](
          (s, _) => s.withRecord(SingletonRecord(Pilot.category)),
          only("pilot"), "not pilot")
        case "EXCLUSION" => ObjectParser.option[Session, Exclusion](_.withRecord(_), Exclusion.parse, "not excluded")
        case "PATH" => ObjectParser.map[Session, File](_.withAssetPath(_), file)
        case "OFFSET" => ObjectParser.map[Session, Offset](_.withAssetOffset(_), offset)
        case fileRegex(cls, name) =>
          val c = Option(cls).fold(Classification.IDENTIFIED)(classification(_))
          val n = Option(name).map(_.trim)
          val k = n.exists(_.equals("<name>"))
          Parser[Session => Session] { p => s =>
            if (p.isEmpty) s else {
              val f = s.assetPath.fold(new File(p))(new File(_, p))
              s.withAsset(Asset(if (k) f.getName.replaceFirst("\\.[a-zA-Z0-9]{1,4}$","") else n.getOrElse(""), f, c, s.assetOffset))
            }
          }
        case "SETTING" => location(Setting.measureParse)
        case "COUNTRY" => location(measureParser(metricCountry, trimmed), metricCountry.assumed.getOrElse(""))
        case "STATE" => location(measureParser(metricState, trimmed))
        case "CONDITION" => record(Condition.category, trimmed)
        case "GROUP" => record(Group.category, trimmed)
        case "STUDY LANGUAGE" => location(measureParser(metricLanguage, trimmed), metricLanguage.assumed.getOrElse(""))
        case "PHOTO MEDIA RELEASE" => Parser(_ => identity _)
        case _ => participant(Participants.parseHeader(name))
      }
    private def header : Parser[CellParser[Session]] =
      Parser[CellParser[Session]] {
        case "" => blankCellParser
        case s => new CellParser(s, parseHeader(s))
      }
    private def parseData(l : List[List[String]], pm : Option[ParticipantMap]) : Seq[Session] = l.zipWithIndex match {
      case h :: l =>
        val p = listAll(header).run(h)
        val line = parseCells(empty, p)
        val pline = pm.fold(line)(m => line.map(_.fillParticipant(m)))
        l.map(pline.run)
      case Nil => Nil
    }
    final def parseCSV(f : File, p : Option[File]) : Future[Seq[Session]] =
      Future(parseData(CSV.parseFile(f), p.map(Participants.parseCSV)))
  }

  def parse(s : File, p : Option[File]) : Future[Int] =
    Sessions.parseCSV(s, p).map(_.length)

  def process(volume : Volume, s : File, p : Option[File])(implicit request : controllers.SiteRequest[_]) : Future[Seq[Container]] =
    Sessions.parseCSV(s, p).flatMap(_.mapAsync(_.populate(volume)))

}
