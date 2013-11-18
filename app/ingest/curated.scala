package ingest

import java.io.File
import java.util.regex.{Pattern=>Regex}
import scala.concurrent.Future
import play.api.libs.Files
import macros._
import dbrary._
import site._
import models._

object Curated {
  import Parse._
  implicit val executionContext = site.context.process

  /* These are all upper-case to allow case-folding insensitive matches.
   * They also must match (in order) the option in the various metrics. */
  private class MetricENUM(metric : Metric[String]) extends ENUM(metric.name) {
    def valueOf(e : Value) = metric.values(e.id)
  }
  private object Gender extends MetricENUM(Metric.Gender) {
    val FEMALE, MALE = Value
  }
  private object Race extends MetricENUM(Metric.Race) {
    val INDIAN, ASIAN, PACIFIC, BLACK, WHITE, MULTIPLE = Value
  }
  private object Ethnicity extends MetricENUM(Metric.Ethnicity) {
    val NONHISPANIC, HISPANIC = Value
  }
  private type RaceEthnicity = (Option[Race.Value], Option[Ethnicity.Value])
  private def parseRaceEthnicity : Parser[RaceEthnicity] = Parser { s =>
    Maybe(s.indexOf('/')).opt.fold {
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

  final case class PopulateException(message : String, target : Option[SitePage] = None) extends IngestException(message)
  object PopulateException {
    def apply(message : String, target : SitePage) : PopulateException = PopulateException(message, Some(target))
  }

  private[this] def check(b : Boolean, t : => PopulateException) : Future[Unit] =
    if (b) Async(()) else Future.failed(t)

  private final case class Subject(id : String, gender : Gender.Value, birthdate : Date, race : Option[Race.Value], ethnicity : Option[Ethnicity.Value], language : Option[String]) extends KeyedData {
    def fields = Seq(id, gender.toString, birthdate.toString, optString(race) + "/" + optString(ethnicity), optString(language))
    def key = id

    private def measures : Seq[(Metric[_],String)] = Seq(
        Metric.Ident -> id
      , Metric.Gender -> Gender.valueOf(gender)
      , Metric.Birthdate -> birthdate.toString) ++
      race.map(Metric.Race -> Race.valueOf(_)) ++
      ethnicity.map(Metric.Ethnicity -> Ethnicity.valueOf(_)) ++
      language.map(Metric.Language -> _)
    def populate(volume : Volume) : Future[models.Record] =
      models.Record.findMeasure(volume, Some(RecordCategory.Participant), Metric.Ident, id).flatMap {
        case Nil =>
          for {
            rec <- Record.create(volume, Some(RecordCategory.Participant))
            _ <- Async.fold(measures.map { case (m,v) =>
              rec.setMeasure(m,v).flatMap(check(_, 
                PopulateException("failed to set measure for subject " + id + " " + m.name + ": " + v, rec)))
            })()
          } yield (rec)
        case Seq(rec) =>
          Async.fold[Unit](measures.map { case (m,v) =>
            rec.measures(m).fold {
              rec.setMeasure(m,v).flatMap(check(_,
                PopulateException("failed to set measure for subject " + id + " " + m.name + ": " + v, rec)))
            } { c =>
              check(c.value.equals(v),
                PopulateException("inconsistent mesaure for subject " + id + " " + m.name + ": " + v + " <> " + c.value, rec))
            }
          })().map(_ => rec)
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

  case class ModelSession(container : models.Container) {
    var last : Option[Offset] = Some(Offset(0))
  }

  private final case class Session(name : String, date : Date, consent : Consent.Value) extends KeyedData {
    def fields = Seq(name, date.toString, consent.toString)
    def key = name

    def populate(volume : Volume)(implicit site : Site) : Future[ModelSession] =
      Container.findName(volume, name).flatMap {
        case Nil =>
          for {
            con <- Container.create(volume, Some(name), Some(date))
            full = con.fullSlot
            _ <- full.change(consent = consent)
          } yield (ModelSession(con))
        case Seq(con) =>
          val full = con.fullSlot
          for {
            _ <- check(con.date.equals(Some(date)),
              PopulateException("inconsistent date for session " + name + ": " + date + " <> " + con.date, full))
            _ <- check(full.consent.equals(consent),
              PopulateException("inconsistent consent for session " + name + ": " + consent + " <> " + full.consent, full))
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
      record.addSlot(session.container.fullSlot).map(_ => ())
  }

  private final case class Asset(name : String, position : Option[Offset], classification : Classification.Value, file : File) extends KeyedData {
    def fields = Seq(name, optString(position), classification.toString, file.getPath)
    def key = file.getPath

    private def fileInfo(file : File) : Asset.FileInfo =
      Asset.FileInfo(file, AssetFormat.getFilename(file.getPath, false)
        .getOrElse(throw PopulateException("no file format found for " + file.getPath)))

    private val transcodedRegex = "^(.*)/transcoded/(.*)-01.mp4$".r
    def info : Asset.Info = {
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
          if (l.length != 1)
            throw PopulateException("missing or ambiguous original " + t.getPath)
          Asset.TimeseriesInfo(file, TimeseriesFormat.Video, probe.duration, fileInfo(l.head))
        case _ =>
          if (path.endsWith(".mp4"))
            throw PopulateException("untranscoded video: " + path)
          fileInfo(file)
      }
    }

    def populate(info : Asset.Info)(implicit site : Site) : Future[models.FileAsset] = {
      /* for now copy and don't delete */
      val infile = store.TemporaryFileCopy(info.file)
      for {
        asset <- info match {
          case Asset.TimeseriesInfo(_, fmt, duration, orig) =>
            populate(orig).flatMap { o =>
              models.Timeseries.create(fmt, classification, duration, infile, Some(o))
            }
          case Asset.FileInfo(_, fmt) =>
            models.FileAsset.create(fmt, classification, infile)
        }
        _ <- SQL("INSERT INTO ingest.asset VALUES (?, ?)").apply(asset.id, info.path).execute
      } yield (asset)
    }
  }
  private object Asset extends ListDataParser[Asset] {
    val headers = makeHeaders("file ?name", "(file ?)?(offset|onset|pos(ition)?)", "(file ?)?class(ification)?", "(file ?)?path")
    def parse : ListParser[Asset] = for {
      name <- listHead(trimmed, "file name")
      pos <- listHead(option(offset), "offset")
      classification <- listHead(enum(Classification, "classification").mapInput(_.toUpperCase), "classification")
      path <- listHead(trimmed.map { p =>
        val f = new java.io.File(if (p.headOption.equals(Some('/'))) "" else "/databrary/stage", p)
        if (!f.isFile) fail("file not found: " + p)
        f
      }, "file path")
    } yield (Asset(name, pos, classification, path))
    def parseOpt : ListParser[Option[Asset]] = for {
      name <- listHead(option(trimmed), "file name")
      pos <- listHead(guard(name.isDefined, option(offset)), "offset")
      classification <- listHead(guard(name.isDefined, enum(Classification, "classification").mapInput(_.toUpperCase)), "classification")
      path <- listHead(guard(name.isDefined, trimmed.map { p =>
        val f = new java.io.File(if (p.headOption.equals(Some('/'))) "" else "/databrary/stage", p)
        if (!f.isFile) fail("file not found: " + p)
        f
      }), "file path")
    } yield (name.map(Asset(_, pos.get, classification.get, path.get)))
    sealed abstract class Info(val file : File, val format : AssetFormat) {
      def path = file.getPath
    }
    final case class FileInfo(override val file : File, override val format : AssetFormat) extends Info(file, format)
    final case class TimeseriesInfo(override val file : File, override val format : TimeseriesFormat, duration : Offset, original : FileInfo) extends Info(file, format)
  }

  private final case class SessionAsset(sessionKey : String, asset : Asset) extends KeyedData {
    def fields = sessionKey +: asset.fields
    def key = asset.key
    def name = sessionKey + "/" + asset.name

    def populate(session : ModelSession)(implicit site : Site) : Future[models.ContainerAsset] = {
      val info = asset.info
      val container = session.container
      val pos = (asset.position, session.last, info) match {
        case (Some(p), _, _) if p.seconds > 0 =>
          session.last = None
          asset.position
        case (Some(Offset(0)), Some(Offset(0)), info : Asset.TimeseriesInfo) =>
          session.last = Some(info.duration)
          asset.position
        case (Some(Offset(p)), Some(e), info : Asset.TimeseriesInfo) if p < 0 =>
          session.last = Some(e + info.duration)
          Some(e)
        case (Some(Offset(p)), _, _) if p < 0 =>
          throw PopulateException("unexpected negative position at asset " + name)
        case (p, _, _) => p
      }
      SQL("SELECT id FROM ingest.asset WHERE file = ?").apply(asset.file.getPath).singleOpt(SQLCols[models.Asset.Id]).flatMap { iid =>
      Async.flatMap[models.Asset.Id,ContainerAsset](iid, ContainerAsset.get(container, _)).flatMap(
      _.fold(ContainerAsset.findName(container, asset.name))(a => Async(Seq(a))).flatMap {
        case Nil =>
          for {
            _ <- check(iid.isEmpty,
              PopulateException("inconsistant container for previously ingested asset " + name))
            a <- asset.populate(info)
            ca <- ContainerAsset.create(container, a, pos, asset.name, None)
          } yield (ca)
        case Seq(ca) => for {
          _ <- check(ca.asset.format == info.format,
            PopulateException("inconsistent format for asset " + name + ": " + info.format.name + " <> " + ca.asset.format.name))
          _ <- check(ca.asset.classification == asset.classification,
            PopulateException("inconsistent classification for asset " + name + ": " + asset.classification + " <> " + ca.asset.classification))
          _ <- info match {
            case ts : Asset.TimeseriesInfo =>
              check(!ca.asset.asInstanceOf[Timeseries].duration.approx(ts.duration),
                PopulateException("inconsistent duration for asset " + name + ": " + ts.duration + " <> " + ca.asset.asInstanceOf[Timeseries].duration))
            case _ => Async(())
          }
          _ <- check(ca.position.equals(pos),
            PopulateException("inconsistent position for asset " + name + ": " + asset.position + " <> " + ca.position))
        } yield (ca)
        case _ =>
          Future.failed(PopulateException("multiple assets for " + name))
      }
      )
      }
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
      (Row.parseHeaders.run _).tupled(h)
      val rows = l.map((Row.parse.run _).tupled)
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

  private def populate(data : Data, volume : Volume)(implicit site : Site) : Future[(Iterable[Record], Iterable[ContainerAsset])] = for {
    subjs <- Async.sequenceValues(data.subjects.mapValues(_.populate(volume)))
    sess <- Async.sequenceValues(data.sessions.mapValues(_.populate(volume)))
    _ <- Async.fold(data.subjectSessions.map { ss =>
      ss.populate(subjs(ss.subjectKey), sess(ss.sessionKey))
    })()
    assets <- Async.sequence(data.assets
      .sortBy(_.asset.position.map(-_.seconds)).map { sa =>
      sa.populate(sess(sa.sessionKey))
    })
  } yield ((subjs.values, assets))

  def preview(f : java.io.File) : String =
    preview(process(CSV.parseFile(f)))

  def populate(f : java.io.File, volume : Volume)(implicit site : Site) : Future[(Iterable[Record], Iterable[ContainerAsset])] =
    Future(process(CSV.parseFile(f))).flatMap(populate(_, volume))
}
