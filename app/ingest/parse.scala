package ingest

import scala.util.control.Exception.catching
import java.util.regex.{Pattern=>Regex}
import org.joda.time
import macros._
import dbrary._
import models._

final case class ParseException(message : String, line : Int = 0, column : Int = 0, context : String = "") extends IngestException(message) {
  override def getMessage =
    if (line > 0 || column > 0)
      "at " + (if (line > 0) line.toString else "?") +
      (if (column != 0) ":" + column.toString else "") + 
      Maybe.bracket(" (", context, ")") +
      ": " + message
    else
      message
}
object ParseException {
  def adjPosition[T](line : Int = 0, column : Int = 0, context : String = "")(r : => T) : T =
    try { r } catch {
      case ParseException(m, l, c, w) =>
	throw ParseException(m, l + line, c + column, Maybe.join(w, ":", context))
    }
}

private[ingest] object Parse {
  def fail[T](e : String) : T = throw ParseException(e)

  sealed abstract class AbstractParser[I,A](parse : I => A) extends (I => A) {
    type Self >: this.type <: AbstractParser[I,A]
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
    p.map[Option[T]](Some(_)).onEmpty(None)
  def const[T](x : T) : Parser[T] = Parser {
    case "" => x
    case s => fail("unexpected: " + s)
  }
  def guard[T](b : Boolean, p : Parser[T]) : Parser[Option[T]] = 
    if (b) p.map[Option[T]](Some(_))
    else const[Option[T]](None)

  private val dateFormat1 = time.format.DateTimeFormat.forPattern("yyyy-MM-dd")
  private val dateFormat2 = time.format.DateTimeFormat.forPattern("MM/dd/yy").withPivotYear((new time.LocalDate).getYear - 49)
  private def dateFormat(fmt : time.format.DateTimeFormatter) : Parser[Date] = Parser(s =>
      fmt.parseLocalDate(s)
    ).failingOn(classOf[IllegalArgumentException])
  val date : Parser[Date] = dateFormat(dateFormat2) | dateFormat(dateFormat1)

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

  class ColumnParser[A](val name : String, parse : Parser[A]) extends ListParser[A]({
    case Nil => fail(name + " expected") /* could also inject fake empty field */
    case x :: l => (ParseException.adjPosition(column = -l.length-1, context = name)(parse(x)), l)
  })

  def listHead[T](p : Parser[T], name : String) : ListParser[T] = new ColumnParser(name, p)

  def listParse_(ps : Seq[ColumnParser[Unit]]) : ListParser[Unit] = ListParser[Unit] { l =>
    ((), ps.foldLeft(l) { (l, sp) => sp(l)._2 })
  }

  def listAll[A](parse : Parser[A]) : ListParser[Seq[A]] = ListParser[Seq[A]] { l =>
    val n = l.length
    (l.zipWithIndex.map { case (s, i) =>
      ParseException.adjPosition(column = i-n)(parse(s))
    }, Nil)
  }
  
  trait ListData {
    def fields : Seq[String]
  }
  abstract class ListDataParser[T <: ListData] {
    final protected def makeHeaders(s : String*) = s.map(Regex.compile(_, Regex.CASE_INSENSITIVE))
    val headers : Seq[Regex]
    def parse : ListParser[T]
    def parseHeaders : ListParser[Unit] =
      listParse_(headers.map(p => new ColumnParser("header (" + p + ")", regex(p, "header"))))
  }


}

private[ingest] class Ingest {
  protected final implicit val executionContext = site.context.process

  protected final val ingestDirectory = new java.io.File("/databrary/stage")

  /* These are all upper-case to allow case-folding insensitive matches.
   * They also must match (in order) the option in the various metrics. */
  protected class MetricENUM(val metric : Metric[String]) extends Parse.ENUM(metric.name) {
    def valueOf(e : Value) = metric.values(e.id)
    def valueParse : Parse.Parser[String] = parse.map(valueOf)
    val measureParse : Parse.Parser[MeasureV[String]] = valueParse.map(new MeasureV(metric, _))
  }

  protected object Gender extends MetricENUM(Metric.Gender) {
    val FEMALE, MALE = Value
  }
  protected object Race extends MetricENUM(Metric.Race) {
    val INDIAN, ASIAN, PACIFIC, BLACK, WHITE, MULTIPLE = Value
  }
  protected object Ethnicity extends MetricENUM(Metric.Ethnicity) {
    val NONHISPANIC, HISPANIC = Value
  }
  protected type RaceEthnicity = (Option[Race.Value], Option[Ethnicity.Value])
}
