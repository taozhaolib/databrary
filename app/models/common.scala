package models

import scala.slick.lifted.{MappedTypeMapper,BaseTypeMapper}

abstract trait TableRow {
  def commit
}

abstract class DBEnum(type_name : String) extends Enumeration {
  implicit val typeMapper = new MappedTypeMapper[Value, String] with BaseTypeMapper[Value] {
    def map(t: Value) = t.toString
    def comap(u: String) = withName(u)
    override def sqlTypeName = Some(type_name)
  }
}
