package models

import scala.language.implicitConversions
import scala.slick.ast.{Node,ProductNode}
import scala.slick.driver.BasicProfile
import scala.slick.lifted._
import scala.slick.session.{PositionedParameters,PositionedResult,Session}
import scala.slick.util.{RecordLinearizer,NaturalTransformation2}
import java.sql.{Timestamp,SQLException}

class CachedVal[T <: AnyRef](init : Session => T) {
  private var x : Option[T] = None
  def apply(db : Session) : T = x.getOrElse(update(init(db)))
  def update(v : T) : T = {
    x = Some(v)
    v
  }
}

object CachedVal {
  def apply[T <: AnyRef](init : Session => T) = new CachedVal(init)
  implicit def implicitGetCached[T <: AnyRef](x : CachedVal[T])(implicit db : Session) : T = x(db)
}

object DBFunctions {
  val currentTimestamp = SimpleFunction.nullary[Timestamp]("transaction_timestamp")
  val ilike = SimpleBinaryOperator[Boolean]("ILIKE")
}

abstract trait TableRow

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

/* It's not clear why Projection is only over Column and not ColumnBase, but this should work: */
final class ColumnPair[T1,T2](_1 : ColumnBase[T1], _2 : ColumnBase[T2])
  extends Tuple2(_1, _2) with ColumnBase[(T1,T2)] with ProductNode with Product {
  lazy val nodeChildren = Vector(Node(_1), Node(_2))
  def getLinearizedNodes : IndexedSeq[Node] = Vector(Node(_1), Node(_2))
  def getResult(profile : BasicProfile, rs : PositionedResult) : (T1,T2) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs)
  )
  def setParameter(profile : BasicProfile, ps : PositionedParameters, value : Option[(T1,T2)]) {
    _1.setParameter(profile, ps, value.map(_._1))
    _2.setParameter(profile, ps, value.map(_._2))
  }
  def updateResult(profile : BasicProfile, rs : PositionedResult, value : (T1,T2)) {
    _1.updateResult(profile, rs, value._1)
    _2.updateResult(profile, rs, value._2)
  }
  // def <>[R](f: ((T1,T2) => R), g: (R => Option[(T1,T2)])) = MappedProjection[R, (T1,T2)](this, { case (p1,p2) => f(p1,p2) }, g)(this)
}

class ViewShape[PackedBase, PackedView, UnpackedBase, UnpackedView](b : PackedView => PackedBase, f : UnpackedBase => UnpackedView, g : UnpackedView => UnpackedBase)(implicit baseShape : Shape[PackedBase, UnpackedBase, _])
  extends IdentityShape[PackedView, UnpackedView]
{
  def linearizer(from : PackedView) = new RecordLinearizer[UnpackedView] {
    private[this] val baseLinearizer = baseShape.linearizer(b(from)).asInstanceOf[RecordLinearizer[UnpackedBase]]
    def getResult(profile : BasicProfile, rs : PositionedResult) : UnpackedView =
      f(baseLinearizer.getResult(profile, rs))
    def updateResult(profile : BasicProfile, rs : PositionedResult, value : UnpackedView) : Unit =
      baseLinearizer.updateResult(profile, rs, g(value))
    def setParameter(profile : BasicProfile, ps : PositionedParameters, value : Option[UnpackedView]): Unit =
      baseLinearizer.setParameter(profile, ps, value.map(g))
    def getLinearizedNodes : IndexedSeq[Node] =
      baseLinearizer.getLinearizedNodes
  }
  def buildPacked(f : NaturalTransformation2[TypeMapper, ({ type L[X] = UnpackedView => X})#L, Column]) = impureShape
}

case class Inet(val ip : String)

object Inet {
  private[this] val inetTypeMapperDelegate = new TypeMapperDelegate[Inet] {
    def zero = Inet("0.0.0.0")
    def sqlType = java.sql.Types.OTHER
    def sqlTypeName = "inet"
    def setValue(v : Inet, p : PositionedParameters) = p.setObject(v.ip, sqlType)
    def setOption(v : Option[Inet], p : PositionedParameters) = p.setObjectOption(v.map(_.ip), sqlType)
    def nextValue(r : PositionedResult) : Inet = {
      val s = r.nextString;
      if (r.rs.wasNull)
        null
      else
        Inet(s)
    }
    def updateValue(v : Inet, r : PositionedResult) = r.updateString(v.ip)
    override def valueToSQLLiteral(v : Inet) = throw new SQLException("Inet literals not (yet) supported")
  }
  implicit val typeMapper = new BaseTypeMapper[Inet] {
    def apply(profile : BasicProfile) = inetTypeMapperDelegate
  }
}
