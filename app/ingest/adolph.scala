package ingest

import java.io.File
import scala.concurrent.Future
import macros._
import dbrary._
import models._
import store.Stage

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

  private def measureMap(m : Measure[_]*) : Map[Int,Measure[_]] =
    Map(m.map(m => (m.metricId.unId, m)) : _*)

  private abstract class Record(val category : RecordCategory) {
    protected val measures : Map[Int,Measure[_]]
    protected def idents : Iterable[Measure[_]]
    type Key
    def key : Key

    override def toString = category.name + ":" + idents.map(m => m.metric.name + ":" + m.datum).mkString(",")

    def getMeasure(m : Metric[_]) : Option[Measure[_]] =
      measures.get(m.id.unId)
    protected final def addMeasure(m : Measure[_]) : Map[Int,Measure[_]] = {
      val i = m.metricId.unId
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
      }	yield (l.headOption)
    def populate(volume : Volume) : Future[models.Record] =
      for {
	ro <- find(volume)
	r <- Async.getOrElse(ro, Record.create(volume, Some(category)))
	_ <- Async.foreach[Measure[_], Unit](measures.values, { m =>
	  r.measures(m.metric).fold {
	    r.setMeasure(m).flatMap(check(_, 
	      PopulateException("failed to set measure for record " + this, r)))
	  } { c =>
	    check(c === m,
	      PopulateException("inconsistent mesaures for record " + this + ": " + m + " <> " + c, r))
	  }
	})
      } yield (r)
  }

  private final case class SingletonRecord(override val category : RecordCategory, measures : Map[Int,Measure[_]] = Map.empty) extends Record(category) {
    def idents = Nil
    type Key = Unit
    def key = ()
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }

  private final case class IdentRecord(override val category : RecordCategory, measures : Map[Int,Measure[_]] = Map.empty) extends Record(category) {
    def idents = getMeasure(Metric.Ident)
    type Key = String
    def key = getMeasure(Metric.Ident).fold(fail(category.name + " ident missing"))(_.datum)
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }

  private final case class TotalRecord(override val category : RecordCategory, measures : Map[Int,Measure[_]] = Map.empty) extends Record(category) {
    def idents = measures.values
    type Key = Map[Int,String]
    def key = measures.mapValues(_.datum)
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }

  private final case class Participant(measures : Map[Int,Measure[_]] = Map.empty)
    extends Record(RecordCategory.Participant) {
    def idents = getMeasure(Metric.Ident) ++ getMeasure(Metric.Info)
    type Key = (String, String)
    def key = (getMeasure(Metric.Ident).fold(fail("participant ID missing"))(_.datum), getMeasure(Metric.Info).fold("")(_.datum))
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }

  private type ParticipantMap = Map[(String, String), Participant]

  private object Participants {
    private final val empty = Participant()
    private def measure[T](m : Parser[MeasureV[T]], nullif : String = "") : Parser[Participant => Participant] =
      ObjectParser.option[Participant,MeasureV[T]](_.withMeasure(_), m, nullif)
    val parseId = measure(measureParser(Metric.Ident, trimmed), null)
    val parseSet = measure(measureParser(Metric.Info, trimmed), null)
    private def parseHeader(name : String) : Parser[Participant => Participant] =
      name match {
	case "SUBJECT ID" => parseId
	case "DATASET" => parseSet
	case "BIRTH DATE" => measure(dateMeasureParser(Metric.Birthdate), null)
	case "GENDER" => measure(Gender.measureParse)
	case "RACE" => measure(Race.measureParse)
	case "ETHNICITY" => measure(Ethnicity.measureParse)
	case "TYPICAL DEVELOPMENT/DISABILITY" => measure(measureParser(Metric.Disability, trimmed), "typical")
	case _ => fail("unknown participant header: " + name)
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

  private final case class Exclusion(measures : Map[Int,Measure[_]] = Map.empty) extends Record(RecordCategory.Exclusion) {
    def idents = getMeasure(Metric.Reason)
    type Key = String
    def key = getMeasure(Metric.Reason).fold(fail("exclusion reason missing"))(_.datum)
    def withMeasure(m : Measure[_]) = copy(measures = addMeasure(m))
  }
  private object Exclusion {
    private object Reason extends MetricENUM(Metric.Reason) {
      val DID_NOT_MEET_INCLUSION_CRITERIA,
	PROCEDURAL_EXPERIMENTER_ERROR,
	FUSSY_TIRED_WITHDREW,
	OUTLIER = Value
    }

    def parse : Parser[Exclusion] =
      Reason.measureParse.map(m => Exclusion(measureMap(m)))
  }

  private object Setting extends MetricENUM(Metric.Setting) {
    val LAB, HOME, MUSEUM, CLASSROOM, OUTDOOR, CLINIC = Value
  }

  private final case class Asset(name : Option[String], path : String) {
    def classification : Classification.Value = Classification.IDENTIFIED
    val info = {
      val f = Stage.file(path)
      if (!f.isFile)
	fail("file not found: " + path)
      val (origFile, file) = Stage.findTranscoded(f) match {
	case Some((Some(o), t)) => (o, t)
	case _ => fail("untranscoded or no original file: " + f)
      }
      val probe = media.AV.probe(file)
      if (!probe.isVideo)
	fail("invalid file format for " + file + ": " + probe.format + " " + probe.streams.mkString(","))
      Asset.TimeseriesInfo(file, AssetFormat.Video, probe.duration, 
	Asset.FileInfo(origFile, AssetFormat.getFilename(origFile.getPath)
	  .getOrElse(fail("no file format found for " + origFile))))

    }
  }

  private object Asset {
    def parse(name : Option[String]) : Parser[Asset] =
      Parser[Asset](Asset(name, _))

    sealed abstract class Info {
      val file : File
      val format : AssetFormat
      final def ingestPath = Stage.path(file)
      def duration : Offset
    }
    final case class FileInfo(val file : File, val format : AssetFormat) extends Info {
      def duration : Offset = Offset.ZERO
    }
    final case class TimeseriesInfo(val file : File, val format : TimeseriesFormat, val duration : Offset, original : FileInfo) extends Info
  }

  private def recordMap(r : Record*) : Map[Int,Record] =
    Map(r.map(r => (r.category.id.unId, r)) : _*)

  private final case class Session(name : String = "", date : Option[Date] = None, consent : Option[Consent.Value] = None, assets : Seq[Asset] = Nil, records : Map[Int,Record] = Map.empty) {
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
      val i = RecordCategory.Participant.id.unId
      val p = records.get(i).fold(Participant())(_.asInstanceOf[Participant])
      copy(records = records.updated(i, f(p)))
    }
    def fillParticipant(pm : ParticipantMap) = {
      val i = RecordCategory.Participant.id.unId
      records.get(i).fold(this) { p =>
	copy(records = records.updated(i,
	  pm.getOrElse(p.asInstanceOf[Participant].key, fail("participant not found: " + p))))
      }
    }
    def withLocation(f : Record => Record) = {
      val i = RecordCategory.Location.id.unId
      val p = records.getOrElse(i, TotalRecord(RecordCategory.Location))
      copy(records = records.updated(i, f(p)))
    }
    def withRecord(r : Record) = {
      val i = r.category.id.unId
      if (records.contains(i))
	fail("duplicate session record: " + r)
      copy(records = records.updated(i, r))
    }
    def withAsset(a : Asset) =
      copy(assets = assets :+ a)
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
    private def parseHeader(name : String) : Parser[Session => Session] =
      name match {
	case "SUBJECT ID" => participant(Participants.parseId)
	case "DATASET" => participant(Participants.parseSet)
	case "TEST DATE" => ObjectParser.map[Session, Date](_.withDate(_), date)
	case "SESSION ID" => ObjectParser.option[Session, String](_.withName(_), trimmed)
	case "RELEASE LEVEL" => ObjectParser.option[Session, Consent.Value](_.withConsent(_), consent)
	case "PILOT" => ObjectParser.option[Session, Unit](
	  (s, _) => s.withRecord(SingletonRecord(RecordCategory.Pilot)),
	  only("pilot"), "not pilot")
	case "EXCLUSION" => ObjectParser.option[Session, Exclusion](_.withRecord(_), Exclusion.parse, "not excluded")
	case "FILE" => ObjectParser.option[Session, Asset](_.withAsset(_), Asset.parse(None))
	case f if f.startsWith("FILE: ") => ObjectParser.option[Session, Asset](_.withAsset(_), Asset.parse(Some(f.stripPrefix("FILE: ").trim)))
	case "SETTING" => location(Setting.measureParse)
	case "COUNTRY" => location(measureParser(Metric.Country, trimmed), "US")
	case "STATE" => location(measureParser(Metric.State, trimmed))
	case "CONDITION" => record(RecordCategory.Condition, trimmed)
	case "GROUP" => record(RecordCategory.Group, trimmed)
	case "STUDY LANGUAGE" => location(measureParser(Metric.Language, trimmed), "English")
	case "PHOTO MEDIA RELEASE" => Parser(_ => identity _)
	case _ => fail("unknown session header: " + name)
      }
    private def header : Parser[CellParser[Session]] =
      Parser[CellParser[Session]] {
	case "" => blankCellParser
	case s => new CellParser(s, parseHeader(s))
      }
    private def parseData(l : List[List[String]], pm : ParticipantMap) : Seq[Session] = l.zipWithIndex match {
      case h :: l =>
	val p = listAll(header).run(h)
	val line = parseCells(empty, p).map(_.fillParticipant(pm))
	l.map(line.run)
      case Nil => Nil
    }
    final def parseCSV(f : File, pm : ParticipantMap) =
      parseData(CSV.parseFile(f), pm)
  }

  def parseFiles(s : File, p : File) : Int =
    Sessions.parseCSV(s, Participants.parseCSV(p)).length
}
