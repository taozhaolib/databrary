package ingest

import java.io.File
import java.sql.Date
import dbrary._
import util._
import models._

object Curated {
  import Parse.listHead
  // subject id,gender,birthdate,race/ethnicity?,session name,test date,consent,file name,file offset,file path

  object Gender extends Parse.Enum("gender") {
    val Female, Male = Value
  }
  object Race extends Parse.Enum("race") {
    val Indian, Asian, Pacific, Black, White, Multiple = Value
  }
  object Ethnicity extends Parse.Enum("ethnicity") {
    val NonHispanic, Hispanic = Value
  }
  type RaceEthnicity = (Option[Race.Value], Option[Ethnicity.Value])
  def parseRaceEthnicity : Parse.Parser[RaceEthnicity] = Parse.Parser { s =>
    maybe(s.indexOf('/'), -1).fold {
      (Race.parseMaybe.map(r => (r, None : Option[Ethnicity.Value])) |
        Ethnicity.parse.map(e => (None, Some(e))))(s)
    } { i =>
      val (r, e) = s.splitAt(i)
      for {
        r <- Race.parseMaybe(r).right
        e <- Ethnicity.parseMaybe(e.tail).right
      } yield ((r,e))
    }
  }
  case class Subject(id : String, gender : Gender.Value, birthdate : Date, race : RaceEthnicity)
  object Subject {
    def parse : Parse.ListParser[Subject] = for {
      id <- listHead(Parse.string, "subject id")
      gender <- listHead(Gender.parse, "gender")
      birthday <- listHead(Parse.date, "birthdate")
      race <- listHead(parseRaceEthnicity, "race/ethnicity")
    } yield (Subject(id, gender, birthday, race))
  }

  case class Session(name : String, date : Date, consent : Consent.Value)
  object Session {
    def parse : Parse.ListParser[Session] = for {
      name <- listHead(Parse.string, "folder name")
      date <- listHead(Parse.date, "session date")
      consent <- listHead(Parse.consent, "consent level")
    } yield (Session(name, date, consent))
  }

  case class Asset(name : String, offset : Option[Offset], path : File)
  object Asset {
    def parse : Parse.ListParser[Asset] = for {
      name <- listHead(Parse.string, "file name")
      offset <- listHead(Parse.maybe(Parse.offset), "offset")
      path <- listHead(Parse.string.cmap { p =>
        val f = new java.io.File(p)
        if (f.isFile) Right(f)
        else Left("file not found: " + p)
      }, "file path")
    } yield (Asset(name, offset, path))
  }

  case class Row(subject : Subject, session : Session, asset : Asset)
  object Row {
    def parse : Parse.ListParser[Row] = for {
      subj <- Subject.parse
      sess <- Session.parse
      asset <- Asset.parse
    } yield (Row(subj, sess, asset))
  }

  def parseCSV(s : String) : Parse.Result[Seq[Row]] =
    CSV.parseString(s).right.flatMap(l => Parse.sequence(l.map(Row.parse.run _)))
}
