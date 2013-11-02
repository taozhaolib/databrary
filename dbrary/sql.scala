package dbrary

import scala.concurrent.{Future,ExecutionContext}
import scala.util.control.Exception.catching
import com.github.mauricio.async.db

trait SQLArgs {
  def args : Seq[Any]
  final def query(query : String)(implicit conn : db.Connection, context : ExecutionContext) : SQLRawsult = {
    val a = args
    new SQLRawsult(
      if (a.isEmpty)
        conn.sendQuery(query)
      else
        conn.sendPreparedStatement(query, a)
    )
  }
}

final class SQLArgseq private (val args : /*=>*/ Seq[Any]) extends SQLArgs {
  import SQLType.put

  def ++(other : SQLArgseq) : SQLArgseq = new SQLArgseq(args ++ other.args)
  def :+[A : SQLType](other : A) : SQLArgseq = new SQLArgseq(args :+ put[A](other))
  def +:[A : SQLType](other : A) : SQLArgseq = new SQLArgseq(put[A](other) +: args)
}

object SQLArgseq {
  import SQLType.put

  def apply = new SQLArgseq(Seq())
  def apply[A1 : SQLType](a1 : A1) = new SQLArgseq(Seq(put[A1](a1)))
  def apply[A1 : SQLType, A2 : SQLType](a1 : A1, a2 : A2) = new SQLArgseq(Seq(put[A1](a1), put[A2](a2)))
}

class SQLResultException(message : String) extends db.exceptions.DatabaseException(message)

class SQLRawsult(result : Future[db.QueryResult])(implicit context : ExecutionContext) {
  private[this] def fail(res : db.QueryResult, msg : String) = throw new SQLResultException(res.statusMessage + ": " + msg)
  private[this] def rows(res : db.QueryResult) = res.rows.getOrElse(fail(res, "no results"))
  private[this] def singleOpt(res : db.QueryResult) = rows(res) match {
    case Seq() => None
    case Seq(r) => Some(r)
    case l => fail(res, "got " + l.length + " rows, expected 1")
  }
  private[this] def single(res : db.QueryResult) = singleOpt(res).getOrElse(fail(res, "got no rows, expected 1"))

  def as[A](parse : SQLRes[A]) : SQLResult[A] = new SQLResult[A](result, parse)

  def list[A](parse : SQLRes[A]) : Future[IndexedSeq[A]] = result.map(rows(_).map(parse(_)))
  def singleOpt[A](parse : SQLRes[A]) : Future[Option[A]] = result.map(singleOpt(_).map(parse(_)))
  def single[A](parse : SQLRes[A]) : Future[A] = result.map(r => parse(single(r)))
}

case class SQLResult[A](result : Future[db.QueryResult], parse : SQLRes[A])(implicit context : ExecutionContext) extends SQLRawsult(result) {
  def map[B](f : A => B) : SQLResult[B] = copy[B](parse = parse.map(f))
  def list : Future[IndexedSeq[A]] = list(parse)
  def singleOpt : Future[Option[A]] = singleOpt(parse)
  def single : Future[A] = single(parse)
}

trait SQLRes[A] extends (db.RowData => A) {
  parent =>

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
  val identity : SQLRes[db.RowData] = new SQLRes[db.RowData] {
    def apply(r : db.RowData) : db.RowData = r
  }

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

abstract sealed class SQLCols[A] protected (n : Int, f : Iterator[Any] => A) extends SQLResub[A](n, l => f(l.iterator)) {
  // def ~+[C : SQLType] : SQLCols[_]
}

object SQLCols {
  def get[A](i : Iterator[Any])(implicit t : SQLType[A]) : A = t.get(i.next)

  def apply = new SQLCols0
  def apply[C1 : SQLType] = new SQLCols1[C1]
  def apply[C1 : SQLType, C2 : SQLType] = new SQLCols2[C1, C2]
}

import SQLCols.get

/* talk about boilerplate, but this sort of arity overloading seems to be ubiquitous in scala */
final class SQLCols0
  extends SQLCols[Unit](0, i => ()) {
  def map[A](f : => A) : SQLResub[A] = map[A]((_ : Unit) => f)
}
final class SQLCols1[C1 : SQLType]
  extends SQLCols[C1](1, i => get[C1](i)) {
}
final class SQLCols2[C1 : SQLType, C2 : SQLType]
  extends SQLCols[(C1,C2)](2, i => (get[C1](i), get[C2](i))) {
  def map[A](f : (C1,C2) => A) : SQLResub[A] = map[A](f.tupled)
}
final class SQLCols3[C1 : SQLType, C2 : SQLType, C3 : SQLType]
  extends SQLCols[(C1,C2,C3)](3, i => (get[C1](i), get[C2](i), get[C3](i))) {
  def map[A](f : (C1,C2,C3) => A) : SQLResub[A] = map[A](f.tupled)
}
