package ingest

import scala.util.control.Exception.catching
import java.sql.Date
import models._

object Parse {
  /** The result of parsing a T: Left(error) or Right(T). */
  type Result[T] = Either[String,T]
  type Parse[T] = String => Result[T]
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
  }

  val string : Parser[String] = Parser(Right(_))

  def maybe[T](p : Parser[T]) : Parser[Option[T]] = Parser { s =>
    if (s.isEmpty) Right(None) else p(s).right.map(Some(_))
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
      enum.values.filter(_.toString.startsWith(s)) match {
        case Seq(r : enum.Value) => Right(r)
        case Seq() => Left("Unknown " + name + ": " + s)
        case _ => Left("Ambiguous " + name + ": " + s)
      }
    } (Right(_))
  }

  abstract class Enum(name : String) extends Enumeration {
    val parse : Parser[Value] = enum(this, name)
    val parseMaybe : Parser[Option[Value]] = maybe(parse)
  }

  def consent : Parser[Consent.Value] =
    enum(Consent, "consent level")

  def offset : Parser[dbrary.Offset] = Parser { s =>
    catching(classOf[java.lang.NumberFormatException]).opt(
      dbrary.Offset(s.toDouble)
    ).toRight("invalid offset (seconds)")
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
      case (r, Nil) => Right(r)
      case (_, l) => Left("unexpected extra fields: " + l.mkString(","))
    }
  }

  def listHead[T](p : Parser[T], name : String) : ListParser[T] = ListParser[T] {
    case Nil => Left(name + " expected")
    case x :: l => p(x).right.map((_, l))
  }

  def sequence[T](l : Seq[Result[T]]) : Result[Seq[T]] =
    l collectFirst { case Left(e) => e } toLeft l.map(_.right.get)
}
