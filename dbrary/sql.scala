package dbrary

import scala.concurrent.Future
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

final class SQLRes[A](private val f : Iterator[Any] => A) {
  def apply(r : db.RowData) : A = {
    val i = r.iterator
    val a = f(i)
    if (i.hasNext)
      throw new db.exceptions.DatabaseException("Not all fields consumed in SQLRes: " + i.next)
    a
  }

  def apply(r : db.ResultSet) : IndexedSeq[A] = r.map(apply _)

  def map[B](g : A => B) = new SQLRes[B](i => g(f(i)))

  def ~[B](o : SQLRes[B]) : SQLRes[(A,B)] = new SQLRes[(A,B)](i => {
    val a = f(i)
    val b = o.f(i)
    (a, b)
  })
}

object SQLRes {
  def apply[A](implicit t : SQLType[A]) = new SQLRes[A](i => t.make(i.next))
}
