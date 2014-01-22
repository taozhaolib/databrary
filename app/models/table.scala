package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import dbrary._
import site._

/** Any class that more-or-less corresponds to a row in a database table.
  * This doesn't have much use, yet, aside from distinguishing model objects. */
private[models] trait TableRow
/** Any class that corresponds to a row in a database table with a primary key. */
private[models] trait TableRowId[+T] extends TableRow {
  /** Primary key. */
  val id : IntId[T]
  override def hashCode = id.unId
  /** Equality based on primary key. */
  final def ===[X >: T](a : TableRowId[X]) = id === a.id
  /** Equality is based on primary key.
    * This assumes that two objects representing the same row are the same (even if they aren't), and also doesn't properly check types due to erasure, so === should be preferred. */
  override def equals(a : Any) = a match {
    case a : TableRowId[T] => ===(a)
    case _ => false
  }
}

/** Factory/helper object for a particular table.  Usually these are used to produce [[TableRow]]s. */
private[models] trait TableView {
  /** Name of the database table. */
  private[models] val table : String
  /** Database OID of the table.  This is useful when dealing with inheritance or other tableoid selections. */
  private[models] lazy val tableOID : Future[Long] =
    SQL("SELECT oid FROM pg_class WHERE relname = ?").apply(table).single(SQLCols[Long])

  /** Type of TableRow this object can generate. */
  private[models] type Row <: TableRow
  /* Description of the database selection to produce a Row. */
  // private[models] val row : Selector[Row]

  protected implicit val fromTable : FromTable = FromTable(table)

  protected def INSERT(args : SQLTerms)(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult =
    SQL("INSERT INTO", table, args.insert)(dbc, exc).apply(args)
  protected def INSERT(args : SQLTerm[_]*)(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult =
    INSERT(SQLTerms(args : _*))(dbc, exc)
  protected def DELETE(args : SQLTerms)(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult =
    SQL("DELETE FROM", table, "WHERE", args.where)(dbc, exc).apply(args)
  protected def DELETE(args : SQLTerm[_]*)(implicit dbc : Site.DB, exc : ExecutionContext) : SQLResult =
    DELETE(SQLTerms(args : _*))(dbc, exc)
}

private[models] abstract class Table[R <: TableRow](private[models] val table : String) extends TableView {
  type Row = R
}
private[models] abstract class TableId[R <: TableRowId[R]](table : String) extends Table[R](table) with HasId[R]

