package ingest

import macros._
import dbrary._
import models._

object Adolph extends Ingest {
  import Parse._

  type ObjectParser[A] = Parser[A => A]
  object ObjectParser {
    def apply[A](f : (A, String) => A) : ObjectParser[A] =
      new Parser[A => A](s => a => f(a, s))
  }

  private class CellParser[A](name : String, parse : ObjectParser[A])
    extends ColumnParser[A => A](name, parse)
  
  private def blankCellParser[A] : CellParser[A] =
    new CellParser[A]("<blank>", const[A => A](identity[A] _))

  private def parseCells[A](ps : Seq[CellParser[A]]) : ListParser[A => A] = {
    @scala.annotation.tailrec
    def pl(a : A => A, ps : Seq[CellParser[A]], l : List[String]) : ListResult[A => A] =
      ps match {
	case Nil => (a, l)
	case p :: ps =>
	  val (af, rl) = p(l)
	  pl(a.andThen(af), ps, rl)
      }
    ListParser[A => A](pl(identity[A] _, ps, _))
  }

  private sealed abstract class DataParser[A] {
    protected val empty : A
    protected def header : Parser[CellParser[A]]
    final def parseData(l : List[List[String]]) : Seq[A] = l.zipWithIndex match {
      case h :: l =>
	val p = (listAll(header).run _).tupled(h)
	l.map((parseCells(p).map(_(empty)).run _).tupled)
      case Nil => fail("no data")
    }
    final def parseCSV(f : java.io.File) : Seq[A] =
      parseData(CSV.parseFile(f))
  }

  
  private def measureParser[T](metric : Metric[T], p : Parser[T]) : Parser[MeasureV[T]] =
    Parser[MeasureV[T]](s => new MeasureV(metric, p(s)))

  private def dateMeasureParser(metric : Metric[Date]) =
    measureParser[Date](metric, date)

  private abstract class Record(category : Option[RecordCategory]) {
    def key : Seq[Measure[_]]
    def measures : Seq[Measure[_]]
    def withMeasure(m : Measure[_]) : Record
  }

  private final case class Participant(id : String, set : String, measures : Seq[Measure[_]])
    extends Record(Some(RecordCategory.Participant)) {
    def key = ??? // (id, set)
    def withId(i : String) =
      if (id != null && !id.equals(i))
	fail("participant ID already set to '" + id + "' at '" + i + "'")
      else
	copy(id = i)
    def withSet(s : String) =
      if (set != null && !set.equals(s))
	fail("participant set already set to '" + set + "' at '" + s + "'")
      else
	copy(set = s)
    def withMeasure(m : Measure[_]) = copy(measures = m +: measures)
  }

  private object participants extends DataParser[Participant] {
    protected final val empty = Participant(null, null, Nil)
    private def measureParser[T](m : Parser[MeasureV[T]]) : Parser[Participant => Participant] =
      m.map[Participant => Participant](x => _.withMeasure(x))
    private def parseHeader(name : String) : Parser[Participant => Participant] =
      name match {
	case "SUBJECT ID" => ObjectParser[Participant](_.withId(_))
	case "DATASET" => ObjectParser[Participant](_.withSet(_))
	case "BIRTH DATE" => measureParser(dateMeasureParser(Metric.Birthdate))
	case "GENDER" => measureParser(Gender.measureParse)
	case "RACE" => measureParser(Race.measureParse).onEmpty(identity[Participant] _)
	case "ETHNICITY" => measureParser(Ethnicity.measureParse).onEmpty(identity[Participant] _)
	case _ => fail("unknown participant header: " + name)
      }
    protected def header : Parser[CellParser[Participant]] =
      Parser[CellParser[Participant]] {
	case "" => blankCellParser
	case s => new CellParser(s, parseHeader(s))
      }
  }
}
