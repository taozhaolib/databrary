package dbrary

import org.postgresql._
import org.postgresql.util._
import java.sql.{Timestamp,Date,SQLException}
import anorm._

class PGObject(pgType : String, pgValue : String) extends PGobject {
  setType(pgType)
  setValue(pgValue)
}

class PGType[A](pgType : String, f : String => A, g : A => String) {
  implicit val column : Column[A] = Column.nonNull[A] { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case obj: PGobject if obj.getType == pgType => Right(f(obj.getValue))
      case obj: PGobject => Left(TypeDoesNotMatch("Incorrect type of object " + value + ":" + obj.getType + " for column " + qualified + ":" + pgType))
      case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to PGobject for column " + qualified))
    }
  }

  implicit val statement : ToStatement[A] = new ToStatement[A] {
    def set(s: java.sql.PreparedStatement, index: Int, a: A) =
      s.setObject(index, new PGObject(pgType, g(a)))
  }
}

object Anorm {
  implicit val columnTimestamp : Column[Timestamp] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case ts: Timestamp => Right(ts)
      case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to Timestamp for column " + qualified))
    }
  }

  implicit val columnDate : Column[Date] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case d: Date => Right(d)
      case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to Date for column " + qualified))
    }
  }

  def toStatementMap[A,S](f : A => S)(implicit ts : ToStatement[S]) : ToStatement[A] = new ToStatement[A] {
    def set(s: java.sql.PreparedStatement, index: Int, a: A) =
      ts.set(s, index, f(a))
  }
  def columnMap[A,S](f : S => A)(implicit c : Column[S]) : Column[A] = Column(
    c(_, _).map(f(_))
  )
}

case class Inet(ip : String)
object Inet extends PGType[Inet]("inet", new Inet(_), _.ip)

abstract class PGRangeBase[A] {
  def toString : String
}
class PGRangeEmpty[A] extends PGRangeBase[A] {
  override def toString = "empty"
}
class PGRange[A](val li : Boolean, val lb : Option[A], val ub : Option[A], val ui : Boolean) extends PGRangeBase[A] {
  override def toString = (if (li) "[" else "(") + lb.map(_.toString).getOrElse("") + "," + ub.map(_.toString).getOrElse("") + (if (ui) "]" else ")")
}
class PGRangeCanonical[A](lbi : A, ube : A) extends PGRange[A](true, Some(lbi), Some(ube), false)
class PGRangeConstructor[A](f : String => A) {
  def apply(s : String) : PGRangeBase[A] = {
    if (s == "empty" || s.isEmpty)
      return new PGRangeEmpty[A]
    val li = s.head match {
      case '[' => true
      case '(' => false
      case _ => throw new SQLException("Invalid range: " + s)
    }
    val c = s.indexOf(',', 1)
    if (c < 0)
      throw new SQLException("Invalid range: " + s)
    val lb = if (c == 1) None else Some(f(s.substring(1,c)))
    val l = s.size
    val ub = if (c == l-2) None else Some(f(s.substring(c+1,l-1)))
    val ui = s.last match {
      case ']' => true
      case ')' => false
      case _ => throw new SQLException("Invalid range: " + s)
    }
    new PGRange[A](li, lb, ub, ui)
  }
}

class PGrange[A](pgType : String, f : String => A) extends PGType[PGRangeBase[A]](pgType, new PGRangeConstructor[A](f).apply _, _.toString)
