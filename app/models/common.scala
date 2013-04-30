package models

import scala.slick.driver.BasicProfile
import scala.slick.lifted.{TypeMapperDelegate,BaseTypeMapper}
import scala.slick.session.{PositionedParameters,PositionedResult}

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
    def nextValue(r : PositionedResult) : Value = withName(r.nextString)
    def updateValue(v : Value, r : PositionedResult) = r.updateString(v.toString)
  }
  implicit val typeMapper = new BaseTypeMapper[Value] {
    def apply(profile : BasicProfile) = typeMapperDelegate
  }
}
