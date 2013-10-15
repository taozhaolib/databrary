package ingest

import java.io.File
import java.sql.Date
import java.util.regex.{Pattern=>Regex}
import play.api.libs.Files
import dbrary._
import site._
import models._

object Curated {
  import Parse._

  /* These are all upper-case to allow case-folding insensitive matches.
   * They also must match (in order) the option in the various metrics. */
  private class MetricENUM(metric : MetricT[String]) extends ENUM(metric.name) {
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
    maybe(s.indexOf('/'), -1).fold {
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

  private final case class Subject(id : String, gender : Gender.Value, birthdate : Date, race : Option[Race.Value], ethnicity : Option[Ethnicity.Value], language : Option[String]) extends KeyedData {
    def fields = Seq(id, gender.toString, birthdate.toString, optString(race) + "/" + optString(ethnicity), optString(language))
    def key = id

    private def measures : Seq[(Metric,String)] = Seq(
        Metric.Ident -> id
      , Metric.Gender -> Gender.valueOf(gender)
      , Metric.Birthdate -> birthdate.toString) ++
      race.map(Metric.Race -> Race.valueOf(_)) ++
      ethnicity.map(Metric.Ethnicity -> Ethnicity.valueOf(_)) ++
      language.map(Metric.Language -> _)
    def populate(volume : Volume)(implicit site : Site) : models.Record =
      models.Record.findMeasure(volume, Some(RecordCategory.Participant), Metric.Ident, id) match {
        case Nil =>
          val rec = Record.create(volume, Some(RecordCategory.Participant))
          measures.foreach { case (m,v) =>
            if (!rec.setMeasure(m,v))
              throw PopulateException("failed to set measure for subject " + id + " " + m.name + ": " + v, rec)
          }
          rec
        case Seq(rec) =>
          measures.foreach { case (m,v) =>
            rec.measure(m).fold {
              if (!rec.setMeasure(m,v))
                throw PopulateException("failed to set measure for subject " + id + " " + m.name + ": " + v, rec)
            } { c =>
              if (!c.value.equals(v))
                throw PopulateException("inconsistent mesaure for subject " + id + " " + m.name + ": " + v + " <> " + c.value, rec)
            }
          }
          rec
        case _ =>
          throw PopulateException("multiple records for subject " + id)
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

  private final case class Session(name : String, date : Date, consent : Consent.Value) extends KeyedData {
    def fields = Seq(name, date.toString, consent.toString)
    def key = name

    def populate(volume : Volume)(implicit site : Site) : models.Container =
      Container.findName(volume, name) match {
        case Nil =>
          val con = Container.create(volume, Some(name), Some(date))
          con.fullSlot.change(consent = consent)
          con
        case Seq(con) =>
          if (!con.date.equals(Some(date)))
            throw PopulateException("inconsistent date for session " + name + ": " + date + " <> " + con.date, con)
          if (!con.fullSlot.consent.equals(consent))
            throw PopulateException("inconsistent consent for session " + name + ": " + consent + " <> " + con.fullSlot.consent, con)
          con
        case _ =>
          throw PopulateException("multiple containers for session " + name)
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
    def populate(record : Record, container : Container)(implicit site : Site) =
      record.addSlot(container.fullSlot)
  }

  private final case class Asset(name : String, position : Option[Offset], classification : Classification.Value, file : File) extends KeyedData {
    def fields = Seq(name, optString(position), classification.toString, file.getPath)
    def key = file.getPath

    def info(implicit db : Site.DB) : Asset.Info =
      AssetFormat.getFilename(file.getPath, true).fold {
        throw PopulateException("no file format found for " + file.getPath)
      } {
        case fmt : TimeseriesFormat =>
          val probe = media.AV.probe(file)
          if (!probe.isVideo)
            throw PopulateException("invalid format for timeseries " + file.getPath + ": " + probe.format + " " + probe.streams.mkString(","))
          Asset.TimeseriesInfo(fmt, probe.duration)
        case fmt =>
          Asset.FileInfo(fmt)
      }

    def populate(info : Asset.Info)(implicit site : Site) : models.Asset = {
      /* for now copy and don't delete */
      val infile = store.TemporaryFileCopy(file)
      info match {
        case Asset.TimeseriesInfo(fmt, duration) =>
          models.Timeseries.create(fmt, classification, duration, infile)
        case Asset.FileInfo(fmt) =>
          models.FileAsset.create(fmt, classification, infile)
      }
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
    sealed abstract class Info(val format : AssetFormat)
    final case class FileInfo(override val format : AssetFormat) extends Info(format)
    final case class TimeseriesInfo(override val format : TimeseriesFormat, duration : Offset) extends Info(format)
  }

  private final case class SessionAsset(sessionKey : String, asset : Asset) extends KeyedData {
    def fields = sessionKey +: asset.fields
    def key = asset.key
    def name = sessionKey + "/" + asset.name

    def populate(container : Container)(implicit site : Site) : models.ContainerAsset = {
      val info = asset.info
      ContainerAsset.findName(container, asset.name) match {
        case Nil =>
          ContainerAsset.create(container, asset.populate(info), asset.position, asset.name, None)
        case Seq(ca) =>
          if (ca.asset.format != info.format)
            throw PopulateException("inconsistent format for asset " + name + ": " + info.format.name + " <> " + ca.asset.format.name)
          if (ca.asset.classification != asset.classification)
            throw PopulateException("inconsistent classification for asset " + name + ": " + asset.classification + " <> " + ca.asset.classification)
          info match {
            case Asset.TimeseriesInfo(_, duration) if !ca.asset.asInstanceOf[Timeseries].duration.approx(duration) =>
              throw PopulateException("inconsistent duration for asset " + name + ": " + duration + " <> " + ca.asset.asInstanceOf[Timeseries].duration)
            case _ => ()
          }
          if (!ca.position.equals(asset.position))
            throw PopulateException("inconsistent position for asset " + name + ": " + asset.position + " <> " + ca.position)
          ca
        case _ =>
          throw PopulateException("multiple assets for " + name)
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
    , subjectSessions : Iterable[SubjectSession]
    , assets : Iterable[SessionAsset]
    )

  private def process(l : List[List[String]]) : Data = l.zipWithIndex match {
    case h :: l =>
      (Row.parseHeaders.run _).tupled(h)
      val rows = l.map((Row.parse.run _).tupled)
      val subjs = collect(rows.map(_.subject))
      val sess = collect(rows.map(_.session))
      val assets = collect(rows.flatMap(r => r.asset.map(SessionAsset(r.session.key, _))))
      Data(subjs, sess, rows.map(r => SubjectSession(r.subject.key, r.session.key)), assets.values)
    case Nil => fail("no data")
  }

  private def preview(data : Data) : String =
    "Import contains:\n" +
      data.subjects.size + " subjects: " + data.subjects.keys.mkString(",") + "\n" +
      data.sessions.size + " sessions: " + data.sessions.keys.mkString(",") + "\n" +
      data.assets.size + " files"

  private def populate(data : Data, volume : Volume)(implicit site : Site) : (Iterable[Record], Iterable[ContainerAsset]) = {
    val subjs = data.subjects.mapValues(_.populate(volume))
    val sess = data.sessions.mapValues(_.populate(volume))
    data.subjectSessions.foreach { ss =>
      ss.populate(subjs(ss.subjectKey), sess(ss.sessionKey))
    }
    val assets = data.assets.map { sa =>
      sa.populate(sess(sa.sessionKey))
    }
    (subjs.values, assets)
  }

  def preview(f : java.io.File) : String =
    preview(process(CSV.parseFile(f)))

  def populate(f : java.io.File, volume : Volume)(implicit site : Site) : (Iterable[Record], Iterable[ContainerAsset]) =
    populate(process(CSV.parseFile(f)), volume)
}
