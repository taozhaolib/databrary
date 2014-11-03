package models

import scala.collection.immutable.IntMap
import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import macros._
import dbrary._
import site._

/** Any class that more-or-less corresponds to a row in a database table. */
private[models] trait TableRow {
  private[models] def sqlKey : SQLTerms
}

/** Any class that corresponds to a row in a database table with a primary key. */
private[models] trait TableRowId[T] extends TableRow with HasId[T] {
  val id : IntId[T]
  def _id : Int = id._id
  private[models] def sqlKey = SQLTerms('id -> _id)
}

/** Factory/helper object for a particular table.  Usually these are used to produce TableRows. */
private[models] trait TableView {
  /** Name of the database table. */
  private[models] def table : String
  protected implicit def fromTable : FromTable = FromTable(table)
  /** Database OID of the table.  This is useful when dealing with inheritance or other tableoid selections. */
  private[models] lazy val tableOID : Long = async.AWAIT {
    SQL("SELECT oid FROM pg_class WHERE relname = ?").apply(table).single(SQLCols[Long])
  }

  /** Type of TableRow this object can generate. */
  private[models] type Row // <: TableRow
  /* Description of the database selection to produce a Row. */
  // private[models] val row : Selector[Row]
  private[models] def fixed(r : Row with TableRow) = r.sqlKey.values.map(_ => r)

  protected def INSERT(args : SQLTerms, returning : String = "")(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult =
    SQL("INSERT INTO", table, args.insert, Maybe.bracket("RETURNING ", returning))(dbc, exc).apply(args)
  protected def INSERT(args : SQLTerm[_]*)(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult =
    INSERT(SQLTerms(args : _*))(dbc, exc)
  protected def DELETE(args : SQLTerms, returning : String = "")(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult =
    SQL("DELETE FROM ONLY", table, "WHERE", args.where, Maybe.bracket("RETURNING ", returning))(dbc, exc).apply(args)
  protected def DELETE(args : SQLTerm[_]*)(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult =
    DELETE(SQLTerms(args : _*))(dbc, exc)
}

private[models] abstract class Table[R] protected (private[models] val table : String) extends TableView {
  type Row = R
}
private[models] abstract class TableId[R <: TableRowId[R]] protected (table : String) extends Table[R](table) with ProvidesId[R]

final class TableIdMap[R <: TableRowId[R]] protected (map : IntMap[R]) extends IntIdMap[R,R](map) {
  override def iterator = map.valuesIterator.map(o => (o.id, o))
  def +(r : R) : TableIdMap[R] = new TableIdMap[R](map + (r.id._id -> r))
  override def empty = new TableIdMap[R](map.empty)
  override def keysIterator = map.valuesIterator.map(_.id)
}

private[models] object TableIdMap {
  def apply[R <: TableRowId[R]](elems : R*) : TableIdMap[R] =
    new TableIdMap[R](IntMap.apply[R](elems.map(r => r.id._id -> r) : _*))
  def empty[R <: TableRowId[R]] : TableIdMap[R] =
    new TableIdMap[R](IntMap.empty[R])
}
