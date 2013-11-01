package dbrary

import scala.concurrent.{Future,ExecutionContext}
import scala.util.control.Exception.catching
import com.github.mauricio.async.db

final class SQLArgs private (args : /*=>*/ List[Any]) extends scala.collection.SeqProxy[Any] {
  def self : Seq[Any] = args
  def add[A1](a1 : A1)(implicit t1 : SQLType[A1]) = new SQLArgs(args :+ t1.put(a1))
  def apply(query : String)(implicit conn : db.Connection) : Future[db.QueryResult] =
    conn.sendPreparedStatement(query,
      try {
        args
      } catch { /* NOTUSED */
        case e : SQLTypeMismatch => throw e.amend("executing " + query)
      }
    )
}

object SQLArgs {
  def apply = new SQLArgs(List())
  def apply[A1](a1 : A1)(implicit t1 : SQLType[A1]) = new SQLArgs(List(t1.put(a1)))
  def apply[A1,A2](a1 : A1, a2 : A2)(implicit t1 : SQLType[A1], t2 : SQLType[A2]) = new SQLArgs(List(t1.put(a1), t2.put(a2)))
}

class SQLResultException(message : String) extends db.exceptions.DatabaseException(message)

case class SQLResult[A](result : db.QueryResult, parse : SQLRes[A]) {
  private def fail(msg : String) = throw new SQLResultException(result.statusMessage + ": " + msg)
  lazy val rows : IndexedSeq[db.RowData] = result.rows.getOrElse(fail("no results"))
  def list : IndexedSeq[A] = rows.map(parse(_))
  def singleOpt : Option[A] = rows match {
    case Seq() => None
    case Seq(r) => Some(parse(r))
    case _ => fail("got " + rows.length + " rows, expected 1")
  }
  def single : A = singleOpt.getOrElse(fail("got no rows, expected 1"))
}

trait SQLRes[A] extends (db.RowData => A) {
  parent =>

  def apply(r : Future[db.QueryResult])(implicit executionContext : ExecutionContext) : Future[SQLResult[A]] = r.map(SQLResult(_, this))

  def map[B](f : A => B) : SQLRes[B] = new SQLRes[B] {
    def apply(r : db.RowData) : B = f(parent.apply(r))
  }

  def flatMap[B](f : A => SQLRes[B]) : SQLRes[B] = new SQLRes[B] {
    def apply(r : db.RowData) : B = f(parent.apply(r)).apply(r)
  }

  def ~[B](right : SQLRes[B]) : SQLRes[(A,B)] = new SQLRes[(A,B)] {
    def apply(r : db.RowData) : (A,B) = (parent.apply(r), right.apply(r))
  }

  def ? : SQLRes[Option[A]] = new SQLRes[Option[A]] {
    def apply(r : db.RowData) : Option[A] =
      catching(classOf[SQLUnexpectedNull]).opt(parent.apply(r))
  }
}

object SQLRes {
  def apply[A](f : db.RowData => A) : SQLRes[A] = new SQLRes[A] {
    def apply(r : db.RowData) : A = f(r)
  }

  def apply[A : SQLType](column : Int) : SQLRes[A] = new SQLRes[A] {
    def apply(r : db.RowData) : A = SQLType.get[A](r, column)
  }
  def apply[A : SQLType](column : String) : SQLRes[A] = new SQLRes[A] {
    def apply(r : db.RowData) : A = SQLType.get[A](r, column)
  }
}

class SQLResub[A](val arity : Int, val get : IndexedSeq[Any] => A) extends SQLRes[A] {
  def apply(r : db.RowData) : A = {
    if (r.length != arity)
      throw new SQLResultException("got " + r.length + " fields, expecting " + arity)
    get(r)
  }

  override def map[B](f : A => B) = new SQLResub[B](arity, l => f(get(l)))

  def ~[B](b : SQLResub[B]) : SQLResub[(A,B)] =
    new SQLResub[(A,B)](arity + b.arity, { l =>
      val (la, lb) = l.splitAt(arity) 
      (get(la), b.get(lb))
    })

  override def ? : SQLResub[Option[A]] = new SQLResub[Option[A]](arity, l =>
    catching(classOf[SQLUnexpectedNull]).opt(get(l)))
}

object SQLResub {
  // def get[A](i : Iterator[Any])(implicit t : SQLType[A]) : A = t.get(i.next)
}

abstract sealed class SQLCols[A] protected (n : Int, f : Iterator[Any] => A) extends SQLResub[A](n, l => f(l.iterator)) {
  def ~+[C : SQLType] : SQLCols[_]
}

object SQLCols {
  def empty = new SQLCols0
  def apply[C1 : SQLType] = new SQLCols1[C1]
  def apply[C1 : SQLType, C2 : SQLType] = new SQLCols2[C1,C2]
  def apply[C1 : SQLType, C2 : SQLType, C3 : SQLType] = new SQLCols3[C1,C2,C3]

  def get[A](i : Iterator[Any])(implicit t : SQLType[A]) : A = t.get(i.next)
}

import SQLCols.get

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
final class SQLCols0
  extends SQLCols[Unit](0, i => ()) {
  def map[A](f : => A) : SQLResub[A] = map[A]((_ : Unit) => f)
  def ~+[C1 : SQLType] = new SQLCols1[C1]
}
final class SQLCols1[C1 : SQLType]
  extends SQLCols[C1](1, i => get[C1](i)) {
  def ~+[C2 : SQLType] = new SQLCols2[C1,C2]
}
final class SQLCols2[C1 : SQLType, C2 : SQLType]
  extends SQLCols[(C1,C2)](2, i => (get[C1](i), get[C2](i))) {
  def map[A](f : (C1,C2) => A) : SQLResub[A] = map[A](f.tupled)
  def ~+[C3 : SQLType] = new SQLCols3[C1,C2,C3]
}
final class SQLCols3[C1 : SQLType, C2 : SQLType, C3 : SQLType]
  extends SQLCols[(C1,C2,C3)](3, i => (get[C1](i), get[C2](i), get[C3](i))) {
  def map[A](f : (C1,C2,C3) => A) : SQLResub[A] = map[A](f.tupled)
  def ~+[C4 : SQLType] = ???
}
