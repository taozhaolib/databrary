package models

import play.api.mvc.{PathBindable,QueryStringBindable}
import anorm._
import dbrary._
import util._

/** A variable with a default value depending on some state.
  * @tparam T the type of the variable/value
  * @tparam S the type of state required to access/compute the value.
  * @param init function to compute the initial default value from state
  */
class CachedVal[T, S](init : S => T) {
  private var x : Option[T] = None
  /** (Compute and) return the current value from a state. */
  def apply(state : S) : T = synchronized(x.getOrElse(update(init(state))))
  /** Change the value, discarding any current computation or future state. */
  def update(v : T) : T = {
    x = Some(v)
    v
  }
}
object CachedVal {
  import scala.language.implicitConversions
  /** Constructor. */
  def apply[T, S](init : S => T) = new CachedVal[T,S](init)
  /** Evaluate the cached value from an implicit state. */
  implicit def implicitGetCached[T, S](x : CachedVal[T, S])(implicit s : S) : T = x(s)
}

/** Wrap identifiers and tag them with a particular type. This is primarily useful to tag primary keys with a specific type corresponding to the source table.
  * @tparam I the type of the identifier
  * @tparam T the tag
  */
class GenericId[I,+T](val unId : I) {
  // I don't understand why this is necessary (and it's also not quite right with inheritance):
  def equals(i : GenericId[I,_]) = i.unId equals unId
  def ==(i : GenericId[I,_]) = i.unId == unId
  def !=(i : GenericId[I,_]) = !(this == i)
  override def hashCode = unId.hashCode
  override def toString = "Id(" + unId.toString + ")"
}
/** [[GenericId]] specific to integers.  The most common (only?) type of identifier we have. */
final class IntId[+T](unId : Int) extends GenericId[Int,T](unId) {
  override def hashCode = unId
  /** Forcibly coerce to a different type. */
  private[models] def coerce[A] = new IntId[A](unId)
}
object IntId {
  def apply[T](i : Int) = new IntId[T](i)
  // The normal family of conversions for database and web i/o:
  implicit def pathBindable[T] : PathBindable[IntId[T]] = PathBindable.bindableInt.transform(apply[T] _, _.unId)
  implicit def queryStringBindable[T] : QueryStringBindable[IntId[T]] = QueryStringBindable.bindableInt.transform(apply[T] _, _.unId)
  implicit def statement[T] : ToStatement[IntId[T]] = dbrary.Anorm.toStatementMap[IntId[T],Int](_.unId)
  implicit def column[T] : Column[IntId[T]] = dbrary.Anorm.columnMap[IntId[T],Int](apply[T] _)
}
/** Any class (usually a singleton object) which provides an Id type. */
private[models] trait HasId[+T] {
  type Id = IntId[T]
  /** Create an [[Id]] value. */
  def asId(i : Int) : Id = new IntId[T](i)
}

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
  private[models] val row : SelectParser[Row]
  /** Shorthand for `row.select`, which provides the SELECT expressions. */
  private[models] final def * : String = row.select
  /** The full SELECT FROM source tables required for the row, which may include additional joins. */
  private[models] val src : String = table

  private[this] def unwords(s : String*) = s.mkString(" ")
  protected def SELECT(q : String*) : SimpleSql[Row] = 
    SQL(unwords(Seq("SELECT", *, "FROM", src) ++ q : _*)).using(row)
  protected def JOIN[A](s : SelectParser[A], q : String*) : SimpleSql[Row ~ A] = {
    val j = row ~ s
    SQL(unwords(Seq("SELECT", j.select, "FROM", src) ++ q : _*)).using(j)
  }
  protected def JOIN(t : TableView, q : String*) : SimpleSql[Row ~ t.Row] =
    JOIN(t.row, Seq("JOIN", t.src) ++ q : _*)

  import scala.language.implicitConversions
  /** Convenient creation of column names for this table from symbols. */
  protected implicit def tableColumn(col : Symbol) = SelectColumn(table, col.name)
}

private[models] abstract class Table[R <: TableRow](private[models] val table : String) extends TableView {
  type Row = R
}
private[models] abstract class TableId[R <: TableRowId[R]](table : String) extends Table[R](table) with HasId[R]

/** Parameters (names and values) that may be passed to SQL queries. */
private[models] final class SQLArgs private (private val args : Seq[SQLArgs.Arg]) extends scala.collection.SeqProxy[SQLArgs.Arg] {
  def self = args
  def ++(other : SQLArgs) = new SQLArgs(args ++ other.args)

  /** Terms appropriate for INSERT INTO statements.
    * @returns `(arg, ...) VALUES ({arg}, ...)`
    */
  def insert = {
    val names = args.map(_._1.name)
    names.mkString("(", ", ", ")") + " VALUES " + names.mkString("({", "}, {", "})")
  }
  /** Terms appropriate for UPDATE or WHERE statements.
    * @param sep separator string, ", " for UPDATE (default), " AND " for WHERE
    * @returns `arg = {arg} sep ...`
    */
  def set(sep : String = ", ") =
    args.map(_._1.name).map(n => n + " = {" + n + "}").mkString(sep)
}
private[models] object SQLArgs {
  /** A single SQL placeholder parameter and its value. */
  type Arg = (Symbol, ParameterValue[_])
  def apply(args : Arg*) = new SQLArgs(args)
}

/** An object with a corresponding page on the site. */
trait SitePage {
  /** The title of the object/page in the hierarchy, which may only make sense within [[pageParent]]. */
  def pageName(implicit site : Site) : String
  /** The object "above" this one (in terms of breadcrumbs and nesting). */
  def pageParent(implicit site : Site) : Option[SitePage]
  /** The URL of the page, usually determined by [[controllers.routes]]. */
  def pageURL : String
}
