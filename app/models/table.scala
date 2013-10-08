package models

import anorm._
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
  /** Equality is based on primary key.  This assumes that two objects representing the same row are the same (even if they aren't). */
  def equals(a : this.type) = a.id == id
}

/** Factory/helper object for a particular table.  Usually these are used to produce [[TableRow]]s. */
private[models] abstract trait TableView {
  /** Name of the database table. */
  private[models] val table : String
  private[this] val _tableOID = CachedVal[Long,Site.DB](SQL("SELECT oid FROM pg_class WHERE relname = {name}").on('name -> table).single(SqlParser.scalar[Long])(_))
  /** Database OID of the table.  This is useful when dealing with inheritance or other tableoid selections. */
  private[models] def tableOID(implicit db : Site.DB) : Long = _tableOID

  /** Type of TableRow this object can generate. */
  private[models] type Row <: TableRow
  /** Description of the database selection to produce a [[Row]]. */
  private[models] val row : Selector[Row]

  import scala.language.implicitConversions
  protected implicit val tableName : FromTable = FromTable(table)
  /** Convenient creation of column names for this table from symbols. */
  protected implicit def tableColumn(col : Symbol) = SelectColumn(col.name)
}

private[models] abstract class Table[R <: TableRow](private[models] val table : String) extends TableView {
  type Row = R
}
private[models] abstract class TableId[R <: TableRowId[R]](table : String) extends Table[R](table) with HasId[R]

