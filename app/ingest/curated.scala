package ingest

import java.io.File
import java.util.regex.{Pattern=>Regex}
import scala.concurrent.Future
import play.api.libs.Files
import macros._
import macros.async._
import dbrary._
import site._
import models._
import store.Stage

object Curated extends Ingest {
  import Parse._

  private def parseRaceEthnicity : Parser[RaceEthnicity] = Parser { s =>
    Maybe(s.indexOf('/')).fold {
      (option(Race.parse).map(r => (r, None : Option[Ethnicity.Value])) |
        Ethnicity.parse.map(e => (None, Some(e))))(s)
    } { i =>
      val (r, e) = s.splitAt(i)
      (option(Race.parse)(r), option(Ethnicity.parse)(e.tail))
    }
  }

  private trait KeyedData extends ListData {
    def key : String
  }
  private def collect[T <: KeyedData](l : Seq[T]) : Map[String,T] =
    l.foldLeft(Map.empty[String,T]) { (m, d) =>
      val k = d.key
      m.get(k).fold(m.updated(k, d)) { o =>
        if (o.equals(d)) m
        else fail("inconsistent values for " + k + ": " + d + " <> " + o)
      }
    }

  private[this] def check(b : Boolean, t : => PopulateException) : Future[Unit] =
    if (b) async.void else Future.failed(t)

  private val metricLanguage = Metric._getName[String]("language")

  private final case class Subject(id : String, gender : Gender.Value, birthdate : Date, race : Option[Race.Value], ethnicity : Option[Ethnicity.Value], language : Option[String]) extends KeyedData {
    def fields = Seq(id, gender.toString, birthdate.toString, optString(race) + "/" + optString(ethnicity), optString(language))
    def key = id

    private def measures : Seq[Measure[_]] = Seq(
        new MeasureV(Metric.Ident, id)
      , new MeasureV(Gender.metric, Gender.valueOf(gender))
      , new MeasureV(Metric.Birthdate, birthdate)) ++
      race.map(r => new MeasureV(Race.metric, Race.valueOf(r))) ++
      ethnicity.map(e => new MeasureV(Ethnicity.metric, Ethnicity.valueOf(e))) ++
      language.map(new MeasureV(metricLanguage, _))
    def populate(volume : Volume) : Future[models.Record] =
      models.Record.findMeasures(volume, Some(RecordCategory.Participant), measures.head).flatMap {
        case Nil =>
          for {
            rec <- Record.create(volume, Some(RecordCategory.Participant))
            _ <- measures foreachAsync { m =>
              rec.setMeasure(m).flatMap(check(_, 
                PopulateException("failed to set measure for subject " + id + ": " + m, rec)))
            }
          } yield (rec)
        case Seq(rec) =>
          measures.foreachAsync({ m =>
            rec.measures(m.metric).fold {
              rec.setMeasure(m).flatMap(check(_,
                PopulateException("failed to set measure for subject " + id + ": " + m, rec)))
            } { c =>
              check(c === m,
                PopulateException("inconsistent mesaure for subject " + id + ": " + m + " <> " + c, rec))
            }
          }, rec)
        case _ =>
          Future.failed(PopulateException("multiple records for subject " + id))
      }
  }
  private object Subject extends ListDataParser[Subject] {
    val headers = makeHeaders("subj(ect)? ?id", "gender|sex", "b(irth)?da(y|te)", "race(/ethnicity)?", "lang(uage)?")
    def parse : ListParser[Subject] = for {
      id <- listHead(trimmed, "subject id")
      gender <- listHead(Gender.parse, "gender")
      birthday <- listHead(date, "birthdate")
      re <- listHead(parseRaceEthnicity, "race/ethnicity")
      lang <- listHead(option(trimmed), "language")
    } yield (Subject(id, gender, birthday, re._1, re._2, lang))
  }

  final case class ModelSession(container : models.Container) {
    var last : Option[Offset] = Some(Offset(0))
  }

  private final case class Session(name : String, date : Date, consent : Consent.Value) extends KeyedData {
    def fields = Seq(name, date.toString, consent.toString)
    def key = name

    def populate(volume : Volume)(implicit site : Site) : Future[ModelSession] =
      Container.findName(volume, name).flatMap {
        case Nil =>
          for {
            con <- Container.create(volume, name = Some(name), date = Some(date))
            _ <- con.setConsent(consent)
          } yield (ModelSession(con))
        case Seq(con) =>
          for {
            _ <- check(con.date.equals(Some(date)),
              PopulateException("inconsistent date for session " + name + ": " + date + " <> " + con.date, con))
            _ <- check(con.consent.equals(consent),
              PopulateException("inconsistent consent for session " + name + ": " + consent + " <> " + con.consent, con))
          } yield (ModelSession(con))
        case _ =>
          Future.failed(PopulateException("multiple containers for session " + name))
      }
  }
  private object Session extends ListDataParser[Session] {
    val headers = makeHeaders("(session|folder) ?(id|name)?", "(test|session) ?date|dot", "consent|sharing")
    def parse : Parse.ListParser[Session] = for {
      name <- listHead(trimmed, "folder name")
      date <- listHead(date, "session date")
      consent <- listHead(option(consent).map(_.getOrElse(Consent.NONE)), "consent level")
    } yield (Session(name, date, consent))
  }

  private final case class SubjectSession(subjectKey : String, sessionKey : String) {
    def populate(record : Record, session : ModelSession)(implicit site : Site) =
      SlotRecord.move(record, session.container, dst = session.container.segment).map(_ => ())
  }

  private final case class Asset(name : String, position : Option[Offset], classification : Classification.Value, file : File) extends KeyedData with ingest.Asset {
    def fields = Seq(name, optString(position), classification.toString, file.getPath)
    def key = file.getPath

    private val transcodedRegex = "(.*)/transcoded/(.*)-01.mp4".r
    def info : ingest.Asset.Info = {
      val path = file.getPath
      path match {
        case transcodedRegex(dir, base) =>
          val probe = media.AV.probe(file)
          if (!probe.isVideo)
            throw PopulateException("invalid format for timeseries " + path + ": " + probe.format + " " + probe.streams.mkString(","))
          val t = new File(dir, base + ".")
          val l = t.getParentFile.listFiles(new java.io.FilenameFilter {
            def accept(d : File, name : String) = name.startsWith(t.getName)
          })
          if (l == null || l.length != 1)
            throw PopulateException("missing or ambiguous original " + t.getPath)
          ingest.Asset.TimeseriesInfo(file, AssetFormat.Video, probe.duration, ingest.Asset.fileInfo(l.head))
        case _ =>
          if (path.endsWith(".mp4"))
            throw PopulateException("untranscoded video: " + path)
          ingest.Asset.fileInfo(file)
      }
    }
  }
  private object Asset extends ListDataParser[Asset] {
    val headers = makeHeaders("file ?name", "(file ?)?(offset|onset|pos(ition)?)", "(file ?)?class(ification)?", "(file ?)?path")
    def parse : ListParser[Asset] = for {
      name <- listHead(trimmed, "file name")
      pos <- listHead(option(offset), "offset")
      classification <- listHead(classification, "classification")
      path <- listHead(trimmed.map { p =>
        val f = Stage.file(p)
        if (!f.isFile) fail("file not found: " + p)
        f
      }, "file path")
    } yield (Asset(name, pos, classification, path))
    def parseOpt : ListParser[Option[Asset]] = for {
      name <- listHead(option(trimmed), "file name")
      pos <- listHead(guard(name.isDefined, option(offset)), "offset")
      classification <- listHead(guard(name.isDefined, classification), "classification")
      path <- listHead(guard(name.isDefined, trimmed.map { p =>
        val f = Stage.file(p)
        if (!f.isFile) fail("file not found: " + p)
        f
      }), "file path")
    } yield (name.map(Asset(_, pos.get, classification.get, path.get)))
  }

  private final case class SessionAsset(sessionKey : String, asset : Asset) extends KeyedData {
    def fields = sessionKey +: asset.fields
    def key = asset.key
    def name = sessionKey + "/" + asset.name

    def populate(session : ModelSession)(implicit request : controllers.SiteRequest[_]) : Future[models.SlotAsset] = {
      val info = asset.info
      val container = session.container
      val pos = (asset.position, session.last) match {
        case (Some(p), _) if p > Offset.ZERO =>
          session.last = None
          Some(p)
        case (Some(Offset(0)), Some(Offset(0))) =>
          session.last = Some(info.duration)
          Some(Offset.ZERO)
        case (Some(Offset(p)), Some(e)) if p < 0 =>
          session.last = Some(e + info.duration)
          Some(e)
        case (Some(Offset(p)), _) if p < 0 =>
          throw PopulateException("unexpected negative position at asset " + name)
        case (Some(p), _) =>
          Some(p)
        case (None, _) =>
          None
      }
      for {
        a <- asset.populate(container.volume)
        s <- a.slot
        sa <- s.fold(
          a.link(container, pos.fold(Segment.full)(p => Segment(p, p + info.duration)))) { sa =>
          for {
            _ <- check(sa.slot.container === container,
              PopulateException("inconsistant container for previously ingested asset " + name))
            _ <- check(sa.slot.segment.lowerBound.equals(pos),
              PopulateException("inconsistent position for asset " + name + ": " + asset.position + "(=> " + pos + ") <> " + sa.slot.segment))
          } yield (sa)
        }
      } yield (sa)
    }
  }

  private final case class Row(subject : Subject, session : Session, asset : Option[Asset]) extends ListData {
    def fields = Seq(subject, session).flatMap(_.fields) ++ asset.fold(Seq("", "", "", ""))(_.fields)
  }
  private object Row extends ListDataParser[Row] {
    val headers = Seq(Subject, Session, Asset).flatMap(_.headers)
    def parse : ListParser[Row] = for {
      subj <- Subject.parse
      sess <- Session.parse
      asset <- Asset.parseOpt
    } yield (Row(subj, sess, asset))
  }

  private final case class Data
    ( subjects : Map[String,Subject]
    , sessions : Map[String,Session]
    , subjectSessions : Seq[SubjectSession]
    , assets : Seq[SessionAsset]
    )

  private def process(l : List[List[String]]) : Data = l.zipWithIndex match {
    case h :: l =>
      Row.parseHeaders.run(h)
      val rows = l.map(Row.parse.run _)
      val subjs = collect(rows.map(_.subject))
      val sess = collect(rows.map(_.session))
      val assets = collect(rows.flatMap(r => r.asset.map(SessionAsset(r.session.key, _))))
      Data(subjs, sess, rows.map(r => SubjectSession(r.subject.key, r.session.key)), assets.values.toSeq)
    case Nil => fail("no data")
  }

  private def preview(data : Data) : String =
    "Import contains:\n" +
      data.subjects.size + " subjects: " + data.subjects.keys.mkString(",") + "\n" +
      data.sessions.size + " sessions: " + data.sessions.keys.mkString(",") + "\n" +
      data.assets.size + " files"

  private def populate(data : Data, volume : Volume)(implicit request : controllers.SiteRequest[_]) : Future[(Iterable[Record], Iterable[SlotAsset])] = for {
    subjs <- data.subjects.mapValuesAsync(_.populate(volume))
    sess <- data.sessions.mapValuesAsync(_.populate(volume))
    _ <- data.subjectSessions.foreachAsync(ss =>
      ss.populate(subjs(ss.subjectKey), sess(ss.sessionKey)))
    assets <- data.assets
      .sortBy(_.asset.position.map(-_))(math.Ordering.Option)
      .mapAsync(sa => sa.populate(sess(sa.sessionKey)))
  } yield ((subjs.values, assets))

  def preview(f : java.io.File) : String =
    preview(process(CSV.parseFile(f)))

  def populate(f : java.io.File, volume : Volume)(implicit request : controllers.SiteRequest[_]) : Future[(Iterable[Record], Iterable[SlotAsset])] =
    Future(process(CSV.parseFile(f))).flatMap(populate(_, volume))
}
