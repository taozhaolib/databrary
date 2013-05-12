package models

import scala.language.implicitConversions
import scala.slick.driver.BasicProfile
import scala.slick.lifted._
import scala.slick.session.{PositionedParameters,PositionedResult}
import java.sql.Timestamp

class CachedVal[T <: AnyRef](init : => T) extends Function0[T] {
  private var x : Option[T] = None
  def apply : T = x.getOrElse(update(init))
  def update(v : T) : T = {
    x = Some(v)
    v
  }
}

object CachedVal {
  def apply[T <: AnyRef](init : => T) = new CachedVal(init)
  implicit def implicitGetCached[T <: AnyRef](x : CachedVal[T]) : T = x()
}

object DBFunctions {
  val currentTimestamp = SimpleFunction.nullary[Timestamp]("transaction_timestamp")
  val ilike = SimpleBinaryOperator[Boolean]("ILIKE")
}

abstract trait TableRow {
  def commit
}

abstract class DBEnum(type_name : String) extends Enumeration {
  private val typeMapperDelegate = new TypeMapperDelegate[Value] {
    def zero = Value(0)
    def sqlType = java.sql.Types.OTHER
    def sqlTypeName = type_name
    def setValue(v : Value, p : PositionedParameters) = p.setObject(v.toString, sqlType)
    def setOption(v : Option[Value], p : PositionedParameters) = p.setObjectOption(v.map(_.toString), sqlType)
    def nextValue(r : PositionedResult) : Value = {
      val s = r.nextString;
      if (r.rs.wasNull)
        null
      else
        withName(s)
    }
    def updateValue(v : Value, r : PositionedResult) = r.updateString(v.toString)
    override def valueToSQLLiteral(v : Value) = {
      if (v eq null)
        "NULL"
      else
        "'" + v.toString + "'"
    }
  }
  implicit val typeMapper = new BaseTypeMapper[Value] {
    def apply(profile : BasicProfile) = typeMapperDelegate
  }
}
