package ingest

import java.io.File
import java.sql.Date
import java.util.regex.{Pattern=>Regex}
import dbrary._
import util._
import models._

object Curated {
  import Parse._

  private def optString[A](v : Option[A]) : String = v.fold("")(_.toString)

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
      for {
        r <- option(Race.parse)(r).right
        e <- option(Ethnicity.parse)(e.tail).right
      } yield ((r,e))
    }
  }

  private trait KeyedData extends ListData {
    def key : String
  }
  private def collect[T <: KeyedData](l : Seq[T]) : Result[Map[String,T]] =
    foldResult(l)(Map.empty[String,T]) { (m, d) =>
      val k = d.key
      m.get(k).fold(result(m.updated(k, d))) { o =>
        if (o.equals(d)) result(m)
        else fail("inconsistent values for " + k + ": " + d + " <> " + o)
      }
    }

  private case class Subject(id : String, gender : Gender.Value, birthdate : Date, race : RaceEthnicity) extends KeyedData {
    def fields = Seq(id, gender.toString, birthdate.toString, optString(race._1) + "/" + optString(race._2))
    def key = id
  }
  private object Subject extends ListDataParser[Subject] {
    val headers = makeHeaders("subj(ect)? ?id", "gender|sex", "b(irth)?da(y|te)", "race(/ethnicity)?")
    def parse : ListParser[Subject] = for {
      id <- listHead(string, "subject id")
      gender <- listHead(Gender.parse, "gender")
      birthday <- listHead(date, "birthdate")
      race <- listHead(parseRaceEthnicity, "race/ethnicity")
    } yield (Subject(id, gender, birthday, race))
  }

  private case class Session(name : String, date : Date, consent : Consent.Value) extends KeyedData {
    def fields = Seq(name, date.toString, consent.toString)
    def key = name
  }
  private object Session extends ListDataParser[Session] {
    val headers = makeHeaders("(session|folder) ?(id|name)?", "(test|session) ?date|dot", "consent|sharing")
    def parse : Parse.ListParser[Session] = for {
      name <- listHead(string, "folder name")
      date <- listHead(date, "session date")
      consent <- listHead(option(consent).map(_.getOrElse(Consent.NONE)), "consent level")
    } yield (Session(name, date, consent))
  }

  private case class SubjectSession(subjectKey : String, sessionKey : String)

  private case class Asset(name : String, offset : Option[Offset], path : File) extends KeyedData {
    def fields = Seq(name, optString(offset), path.getPath)
    def key = path.getPath
  }
  private object Asset extends ListDataParser[Asset] {
    val headers = makeHeaders("file ?name", "(file ?)?(offset|onset|pos(ition)?)", "(file ?)?path")
    def parse : ListParser[Asset] = for {
      name <- listHead(string, "file name")
      offset <- listHead(option(offset), "offset")
      path <- listHead(string.cmap { p =>
        val f = new java.io.File(p)
        if (f.isFile) result(f)
        else fail("file not found: " + p)
      }, "file path")
    } yield (Asset(name, offset, path))
  }

  private case class SessionAsset(sessionKey : String, asset : Asset) extends KeyedData {
    def fields = sessionKey +: asset.fields
    def key = asset.key
  }

  private case class Row(subject : Subject, session : Session, asset : Asset) extends ListData {
    def fields = Seq(subject, session, asset).flatMap(_.fields)
  }
  private object Row extends ListDataParser[Row] {
    val headers = Seq(Subject, Session, Asset).flatMap(_.headers)
    def parse : ListParser[Row] = for {
      subj <- Subject.parse
      sess <- Session.parse
      asset <- Asset.parse
    } yield (Row(subj, sess, asset))
  }

  private case class Data
    ( subjects : Map[String,Subject]
    , sessions : Map[String,Session]
    , subjectSessions : Iterable[SubjectSession]
    , assets : Iterable[SessionAsset]
    )

  private def process(l : List[List[String]]) : Result[Data] = l match {
    case h :: l => 
      for {
        _ <- Row.parseHeaders.run(h).right
        rows <- sequence(l.map(Row.parse.run _)).right
        subjs <- collect(rows.map(_.subject)).right
        sess <- collect(rows.map(_.session)).right
        assets <- collect(rows.map(r => SessionAsset(r.session.key, r.asset))).right
      } yield (Data(subjs, sess, rows.map(r => SubjectSession(r.subject.key, r.session.key)), assets.values))
    case Nil => fail("no data")
  }

  private def preview(data : Data) : String =
    "Import contains:\n" +
      data.subjects.size + " subjects: " + data.subjects.keys.mkString(",") + "\n" +
      data.sessions.size + " sessions: " + data.sessions.keys.mkString(",") + "\n" +
      data.assets.size + " files"

  def preview(f : java.io.File) : Result[String] =
    CSV.parseFile(f, true).right.flatMap(process _).right.map(preview _)
}
