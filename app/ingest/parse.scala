package ingest

import scala.util.control.Exception.catching
import java.sql.Date
import java.util.regex.{Pattern=>Regex}
import models._

final case class ParseException(message : String, line : Int = 0, column : Int = 0) extends IngestException(message) {
  override def getMessage =
    if (line > 0 || column > 0)
      "at " + (if (line > 0) line.toString else "?") +
      ":" + (if (column != 0) column.toString + ":" else "") + 
      " " + message
    else
      message
}
object ParseException {
  def adjPosition[T](line : Int = 0, column : Int = 0)(r : => T) : T =
    try { r } catch {
      case ParseException(m, l, c) => throw ParseException(m, l + line, c + column)
    }
}

object Parse {
  def fail[T](e : String) : T = throw ParseException(e)

  sealed abstract class AbstractParser[I,A](parse : I => A) extends (I => A) {
    type Self <: AbstractParser[I,A]
    protected def copy(parse : I => A) : Self
    final def apply(s : I) : A = parse(s)

    def |(p : Self) : Self = copy { s =>
      catching(classOf[ParseException]).opt(parse(s)) getOrElse p(s)
    }
    def mapInput(f : I => I) : Self = copy { s =>
      parse(f(s))
    }
    def failingOn(exceptions : Class[_]*) : Self = copy { s =>
      catching(exceptions : _*).withApply(e => throw ParseException(e.getMessage).initCause(e)) {
        parse(s)
      }
    }
  }

  case class Parser[A](parse : String => A) extends AbstractParser[String,A](parse) {
    type Self = Parser[A]
    protected def copy(parse : String => A) = Parser[A](parse)

    /* not particularly useful */
    def flatMap[B](f : A => Parser[B]) = Parser[B] { s =>
      f(parse(s))(s)
    }
    def map[B](f : A => B) = Parser[B] { s =>
      f(parse(s))
    }

    def onEmpty(r : => A) = Parser[A] { s =>
      if (s.isEmpty) r else parse(s)
    }
  }

  val trimmed : Parser[String] = Parser(_.trim)

  def option[T](p : Parser[T]) : Parser[Option[T]] = 
    p.map(Some(_) : Option[T]).onEmpty(None)

  private val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val date : Parser[Date] = Parser(s =>
      new java.sql.Date(dateFormat.parse(s).getTime)
    ).failingOn(classOf[java.text.ParseException])

  def enum(enum : Enumeration, name : String) : Parser[enum.Value] =
    Parser(enum.withName(_)).failingOn(classOf[java.util.NoSuchElementException]) |
    Parser(s => enum(s.toInt)).failingOn(classOf[java.lang.NumberFormatException], classOf[java.util.NoSuchElementException]) |
    Parser { s =>
      enum.values.filter(_.toString.startsWith(s)).toSeq match {
        case Seq(r) => r
        case Seq() => fail("Unknown " + name + ": " + s)
        case _ => fail("Ambiguous " + name + ": " + s)
      }
    } onEmpty fail("Missing " + name)

  /** Enumeration that allows case-insensitive parsing assuming all values are upper-case. */
  abstract class ENUM(name : String) extends Enumeration {
    val parse : Parser[Value] = enum(this, name).mapInput(_.toUpperCase)
  }

  val consent : Parser[Consent.Value] =
    enum(Consent, "consent level")

  val offset : Parser[dbrary.Offset] =
    Parser(dbrary.Offset.fromString(_)).failingOn(classOf[java.lang.NumberFormatException])

  def regex(p : Regex, name : String = "pattern") : Parser[Unit] = Parser { s =>
    if (p.matcher(s).matches) () else fail("mismatching " + name + " (" + p + "): " + s)
  }


  def listFail[T](l : List[String], e : String) : T =
    throw ParseException(e, column = -l.length)

  type ListResult[T] = (T, List[String])
  /* A state[List[String]] monad! */
  case class ListParser[A](parse : List[String] => ListResult[A]) extends AbstractParser[List[String], ListResult[A]](parse) {
    type Self = ListParser[A]
    protected def copy(parse : List[String] => ListResult[A]) = ListParser[A](parse)

    def flatMap[B](f : A => ListParser[B]) : ListParser[B] = ListParser[B] { l =>
      parse(l) match { case (r, l) => f(r)(l) }
    }
    def map[B](f : A => B) : ListParser[B] = ListParser[B] { l =>
      parse(l) match { case (r, l) => (f(r), l) }
    }

    def run(l : List[String], line : Int = 0) : A = ParseException.adjPosition(line = line+1, column = l.length+1) {
      parse(l) match {
        case (r, Nil) => r
        case (_, l) => listFail(l, "unexpected extra fields: " + l.mkString(","))
      }
    }
    def onEmpty(r : => A) = ListParser[A] { l =>
      if (l.isEmpty) (r, l) else parse(l)
    }
  }

  def listHead[T](p : Parser[T], name : String) : ListParser[T] = ListParser[T] {
    case Nil => fail(name + " expected")
    case x :: l => (ParseException.adjPosition(column = -l.length-1)(p(x)), l)
  }

  def listParse_(ps : (String, Parser[Unit])*) : ListParser[Unit] = ListParser[Unit] { l =>
    ((), ps.foldLeft(l) { (l, sp) => listHead(sp._2, sp._1)(l)._2 })
  }
  
  trait ListData {
    def fields : Seq[String]
  }
  abstract class ListDataParser[T <: ListData] {
    final protected def makeHeaders(s : String*) = s.map(Regex.compile(_, Regex.CASE_INSENSITIVE))
    val headers : Seq[Regex]
    def parse : ListParser[T]
    def parseHeaders : ListParser[Unit] =
      listParse_(headers.map(p => "header (" + p + ")" -> regex(p, "header")) : _*)
  }
}
