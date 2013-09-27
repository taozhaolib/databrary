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

  /* these are all upper-case to allow case-folding insensitive matches */
  object Gender extends ENUM("gender") {
    val FEMALE, MALE = Value
  }
  object Race extends ENUM("race") {
    val INDIAN, ASIAN, PACIFIC, BLACK, WHITE, MULTIPLE = Value
  }
  object Ethnicity extends ENUM("ethnicity") {
    val NONHISPANIC, HISPANIC = Value
  }
  type RaceEthnicity = (Option[Race.Value], Option[Ethnicity.Value])
  def parseRaceEthnicity : Parser[RaceEthnicity] = Parser { s =>
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
  case class Subject(id : String, gender : Gender.Value, birthdate : Date, race : RaceEthnicity) extends ListData {
    def fields = Seq(id, gender.toString, birthdate.toString, optString(race._1) + "/" + optString(race._2))
  }
  object Subject extends ListDataParser[Subject] {
    val headers = makeHeaders("subj(ect)? ?id", "gender|sex", "b(irth)?da(y|te)", "race(/ethnicity)?")
    def parse : ListParser[Subject] = for {
      id <- listHead(string, "subject id")
      gender <- listHead(Gender.parse, "gender")
      birthday <- listHead(date, "birthdate")
      race <- listHead(parseRaceEthnicity, "race/ethnicity")
    } yield (Subject(id, gender, birthday, race))
  }

  case class Session(name : String, date : Date, consent : Consent.Value) extends ListData {
    def fields = Seq(name, date.toString, consent.toString)
  }
  object Session extends ListDataParser[Session] {
    val headers = makeHeaders("(session|folder) ?(id|name)?", "(test|session) ?date|dot", "consent|sharing")
    def parse : Parse.ListParser[Session] = for {
      name <- listHead(string, "folder name")
      date <- listHead(date, "session date")
      consent <- listHead(option(consent).map(_.getOrElse(Consent.NONE)), "consent level")
    } yield (Session(name, date, consent))
  }

  case class Asset(name : String, offset : Option[Offset], path : File) extends ListData {
    def fields = Seq(name, optString(offset), path.getPath)
  }
  object Asset extends ListDataParser[Asset] {
    val headers = makeHeaders("file ?name", "(file ?)?(offset|onset|pos(ition)?)", "(file ?)?path")
    def parse : ListParser[Asset] = for {
      name <- listHead(string, "file name")
      offset <- listHead(option(offset), "offset")
      path <- listHead(string.cmap { p =>
        val f = new java.io.File(p)
        if (f.isFile) Right(f)
        else Left("file not found: " + p)
      }, "file path")
    } yield (Asset(name, offset, path))
  }

  case class Row(subject : Subject, session : Session, asset : Asset) extends ListData {
    def fields = Seq(subject, session, asset).flatMap(_.fields)
  }
  object Row extends ListDataParser[Row] {
    val headers = Seq(Subject, Session, Asset).flatMap(_.headers)
    def parse : ListParser[Row] = for {
      subj <- Subject.parse
      sess <- Session.parse
      asset <- Asset.parse
    } yield (Row(subj, sess, asset))
  }

  def parseCSV(s : String) : Result[Seq[Row]] =
    CSV.parseString(s).right.flatMap {
      case h :: l => for {
          _ <- Row.parseHeaders.run(h).right
          r <- sequence(l.map(Row.parse.run _)).right
        } yield (r)
      case Nil => Left("no data")
    }
}
