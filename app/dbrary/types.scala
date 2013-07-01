package dbrary

import org.postgresql._
import org.postgresql.util._
import java.sql.Timestamp
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
