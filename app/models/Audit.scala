package models

import util._
import play.api.Play.current
import play.api.db.slick
import slick.DB
import slick.Config.driver.simple._
import scala.slick.ast.{Node,ProductNode}
import scala.slick.driver.BasicProfile
import scala.slick.lifted.{AbstractTable,ColumnBase}
import scala.slick.session.{PositionedResult,PositionedParameters}
import java.sql.Timestamp

object AuditAction extends DBEnum("audit_action") {
  val login, logout, add, change, delete = Value
}

case class Audit[T](who : Int, ip : Inet, action : AuditAction.Value, row : T) {
  def entity(implicit db : Session) = Identity.get(who)
}

class AuditProjection[T](who : Column[Int], ip : Column[Inet], action : Column[AuditAction.Value], row : ColumnBase[T])
  extends Tuple4(who, ip, action, row) with ColumnBase[Audit[T]] with ProductNode with Product {
  private[this] val columns = Vector(who, ip, action, row)
  lazy val nodeChildren = columns.map(Node(_))
  def getLinearizedNodes : IndexedSeq[Node] = nodeChildren
  def getResult(profile : BasicProfile, rs : PositionedResult) = Audit(
    who.getResult(profile, rs), 
    ip.getResult(profile, rs), 
    action.getResult(profile, rs), 
    row.getResult(profile, rs)
  )
  def setParameter(profile : BasicProfile, ps : PositionedParameters, value : Option[Audit[T]]) {
    who.setParameter(profile, ps, value.map(_.who))
    ip.setParameter(profile, ps, value.map(_.ip))
    action.setParameter(profile, ps, value.map(_.action))
    row.setParameter(profile, ps, value.map(_.row))
  }
  def updateResult(profile : BasicProfile, rs : PositionedResult, value : Audit[T]) {
    who.updateResult(profile, rs, value.who)
    ip.updateResult(profile, rs, value.ip)
    action.updateResult(profile, rs, value.action)
    row.updateResult(profile, rs, value.row)
  }
}

abstract class AuditTable[T](protected val table : AbstractTable[T]) extends Table[Audit[T]](maybe(table.tableName).fold("audit")("audit_" + _)) {
  def when = column[Timestamp]("when")
  def who = column[Int]("who")
  def ip = column[Inet]("ip")
  def action = column[AuditAction.Value]("action")

  def row : ColumnBase[T]
  def * = new AuditProjection[T](who, ip, action, row)
  /* def * = new ColumnPair(when ~ who ~ ip ~ action, row) <> (
    (a,r) => Audit(a._1, a._2, a._3, a._4, r), 
    { case Audit(when, who, ip, action, row) => Some(((when, who, ip, action), row)) }
  ) */

  def add(a : Audit[T])(implicit db : Session) =
    * insert a
}

object VoidTable extends Table[Unit]("") {
  def * = new ColumnBase[Unit] with ProductNode /* with NullaryNode */ {
    final def getLinearizedNodes : IndexedSeq[Node] = Vector()
    final def getResult(profile : BasicProfile, rs : PositionedResult) = ()
    final def setParameter(profile : BasicProfile, ps : PositionedParameters, value : Option[Unit]) = ()
    final def updateResult(profile : BasicProfile, rs : PositionedResult, value : Unit) = ()
    final val nodeChildren = Nil
    protected[this] override final def nodeRebuild(ch : IndexedSeq[Node]) : Node = this
  }
}
object Audit extends AuditTable[Unit](VoidTable) {
  def row = table.*
}

object AuditEntity extends AuditTable[Entity](Entity) {
  def id = column[Int]("id")
  def name = column[String]("name")
  def orcid = column[Option[Orcid]]("orcid")
  def row = id ~ name ~ orcid <> (Entity.apply _, Entity.unapply _)
}
