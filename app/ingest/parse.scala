package ingest

import scala.util.control.Exception.catching
import java.sql.Date
import java.util.regex.{Pattern=>Regex}
import models._

object Parse {
  /** The result of parsing a T: Left(error) or Right(T). */
  type Result[T] = Either[String,T]
  type Parse[T] = String => Result[T]

  def result[T](r : T) : Result[T] = Right(r)
  def fail[T](e : String) : Result[T] = Left(e)

  def foldResult[T,R](l : Seq[T])(z : R)(op : (R, T) => Result[R]) =
    l.foldLeft(result(z)) { (r, x) =>
      r.right.flatMap(op(_, x))
    }

  def sequence[T](l : Seq[Result[T]]) : Result[Seq[T]] =
    l collectFirst { case Left(e) => e } toLeft l.map(_.right.get)

  /* A reader monad! */
  case class Parser[A](parse : Parse[A]) extends Parse[A] {
    final def apply(s : String) : Result[A] = parse(s)

    /* not particularly useful */
    def flatMap[B](f : A => Parser[B]) : Parser[B] = Parser[B] { s =>
      parse(s).right.flatMap(f(_)(s))
    }
    def cmap[B](f : A => Result[B]) : Parser[B] = Parser[B] { s =>
      parse(s).right.flatMap(f(_))
    }
    def map[B](f : A => B) : Parser[B] = Parser[B] { s =>
      parse(s).right.map(f)
    }
    def |(p : Parser[A]) = Parser[A] { s =>
      parse(s).left.flatMap(_ => p(s))
    }
    def mapInput(f : String => String) : Parser[A] = Parser[A] { s =>
      parse(f(s))
    }
  }

  val string : Parser[String] = Parser(result(_))

  def option[T](p : Parser[T]) : Parser[Option[T]] = Parser { s =>
    if (s.isEmpty) result(None) else p(s).right.map(Some(_))
  }

  private val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val date : Parser[Date] = Parser { s =>
    catching(classOf[java.text.ParseException]).either(
      new java.sql.Date(dateFormat.parse(s).getTime)
    ).left.map(_.getMessage)
  }

  def enum(enum : Enumeration, name : String) : Parser[enum.Value] = Parser { s =>
    (catching(classOf[java.util.NoSuchElementException]).opt(
      enum.withName(s)
    ) orElse catching(classOf[java.lang.NumberFormatException], classOf[java.util.NoSuchElementException]).opt(
      enum(s.toInt)
    )).fold {
      enum.values.filter(_.toString.startsWith(s)).toSeq match {
        case Seq(r) => result(r)
        case Seq() => fail("Unknown " + name + ": " + s)
        case _ => fail("Ambiguous " + name + ": " + s)
      }
    } (result(_))
  }

  /** Enumeration that allows case-insensitive parsing assuming all values are upper-case. */
  abstract class ENUM(name : String) extends Enumeration {
    val parse : Parser[Value] = enum(this, name).mapInput(_.toUpperCase)
  }

  val consent : Parser[Consent.Value] =
    enum(Consent, "consent level")

  val offset : Parser[dbrary.Offset] = Parser { s =>
    catching(classOf[java.lang.NumberFormatException]).opt(
      dbrary.Offset(s.toDouble)
    ).toRight("invalid offset (seconds)")
  }

  def regex(p : Regex, name : String = "pattern") : Parser[Unit] = Parser { s =>
    if (p.matcher(s).matches) result(()) else fail("mismatching " + name + " (" + p + "): " + s)
  }


  type ListResult[T] = Result[(T, List[String])]
  type ListParse[T] = List[String] => Result[(T, List[String])]
  /* A state[List[String]] monad! */
  case class ListParser[A](parse : ListParse[A]) extends ListParse[A] {
    final def apply(l : List[String]) : ListResult[A] = parse(l)

    def flatMap[B](f : A => ListParser[B]) : ListParser[B] = ListParser[B] { l =>
      parse(l).right flatMap { case (r, l) => f(r)(l) }
    }
    def map[B](f : A => B) : ListParser[B] = ListParser[B] { l =>
      parse(l).right map { case (r, l) => (f(r), l) }
    }
    def run(l : List[String]) : Result[A] = parse(l).right.flatMap {
      case (r, Nil) => result(r)
      case (_, l) => fail("unexpected extra fields: " + l.mkString(","))
    }
  }

  def listHead[T](p : Parser[T], name : String) : ListParser[T] = ListParser[T] {
    case Nil => fail(name + " expected")
    case x :: l => p(x).right.map((_, l))
  }

  def listParse_(ps : (String, Parser[Unit])*) : ListParser[Unit] = ListParser[Unit] { l =>
    foldResult(ps)(l) { (l, sp) =>
      listHead(sp._2, sp._1)(l).right.map(_._2)
    }.right.map(((), _))
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
