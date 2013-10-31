package dbrary

import scala.util.control.Exception.catching
import com.github.mauricio.async.db
import macros._

class SQLTypeMismatch(value : Any, sqltype : SQLType[_], where : String = "")
  extends db.exceptions.DatabaseException("Type mismatch converting " + value.toString + ":" + value.getClass + " to " + sqltype.name + (if (where.isEmpty) "" else " for " + where)) {
  def amend(msg : String) : SQLTypeMismatch = new SQLTypeMismatch(value, sqltype, (if (where.isEmpty) "" else " in ") + msg)
}

abstract class SQLType[A](val name : String, val aClass : Class[A]) {
  parent =>
  def read(s : String) : Option[A]
  def get(x : Any) : Option[A] = x match {
    case a /* : A */ if aClass.isAssignableFrom(x.getClass) => Some(x.asInstanceOf[A])
    case s : String => read(s)
    case _ => None
  }
  def show(a : A) : String = a.toString
  def put(a : A) : Any = a

  final def make(x : Any, where : String = "") : A =
    get(x).getOrElse(throw new SQLTypeMismatch(x, this, where))

  def mapping[B](n : String, bc : Class[B])(f : A => Option[B])(g : B => A) : SQLType[B] =
    new SQLType[B](n, bc) {
      def read(s : String) : Option[B] = parent.read(s).flatMap(f)
      override def get(x : Any) : Option[B] = parent.get(x).flatMap(f)
      override def show(b : B) : String = parent.show(g(b))
      override def put(b : B) : Any = parent.put(g(b))
    }
}

object SQLType {
  def mapping[A,B](name : String, cls : Class[B])(get : A => Option[B])(put : B => A)(implicit base : SQLType[A]) : SQLType[B] =
    base.mapping(name, cls)(get)(put)

  implicit object string extends SQLType[String]("text", classOf[String]) {
    def read(s : String) = Some(s)
  }

  implicit object int extends SQLType[Int]("integer", classOf[Int]) {
    def read(s : String) = maybe.toInt(s)
  }

  implicit object date extends SQLType[Date]("date", classOf[Date]) {
    def read(s : String) =
      catching(classOf[java.lang.IllegalArgumentException]).opt(
        org.joda.time.LocalDate.parse(s))
  }

  implicit def option[A](implicit t : SQLType[A]) : SQLType[Option[A]] =
    new SQLType[Option[A]](t.name + " NULL", classOf[Option[A]]) {
      def read(s : String) : Option[Option[A]] = Option(s).map(t.read _)
      override def get(x : Any) : Option[Option[A]] = Option(x).map(t.get _)
      override def put(a : Option[A]) = a.map(t.put _).orNull
    }

  def get[A](row : db.RowData, name : String)(implicit t : SQLType[A]) =
    t.make(row(name), "column " + name)
  def get[A](row : db.RowData, number : Int)(implicit t : SQLType[A]) =
    t.make(row(number), "column " + number)
}

case class Inet(ip : String)
object Inet {
  implicit val sqlType : SQLType[Inet] =
    SQLType.mapping[String,Inet]("inet", classOf[Inet])(s => Some(Inet(s)))(_.ip)
}
