package dbrary

import scala.util.control.Exception.catching
import com.github.mauricio.async.db
import macros._

class SQLTypeMismatch(value : Any, sqltype : SQLType[_], where : String = "")
  extends db.exceptions.DatabaseException("Type mismatch converting " + (if (value == null) "null" else value.toString + ":" + value.getClass) + " to " + sqltype.name + Maybe.bracket(" for ", where)) {
  def amend(msg : String) : SQLTypeMismatch = new SQLTypeMismatch(value, sqltype, Maybe.bracket("", where, " in ") + msg)
}

class SQLUnexpectedNull(sqltype : SQLType[_], where : String = "") extends SQLTypeMismatch(null, sqltype, where) {
  override def amend(msg : String) : SQLUnexpectedNull = new SQLUnexpectedNull(sqltype, Maybe.bracket("", where, " in ") + msg)
}

abstract class SQLType[A](val name : String, val aClass : Class[A]) {
  parent =>
  def show(a : A) : String = a.toString
  def put(a : A) : Any = a
  def read(s : String) : Option[A]
  def get(x : Any, where : String = "") : A = x match {
    case null => throw new SQLUnexpectedNull(this, where)
    case a /* : A */ if aClass.isAssignableFrom(a.getClass) => a.asInstanceOf[A]
    case s : String => read(s).getOrElse(throw new SQLTypeMismatch(x, this, where))
    case _ => throw new SQLTypeMismatch(x, this, where)
  }

  def transform[B](n : String, bc : Class[B])(f : A => Option[B], g : B => A) : SQLType[B] =
    new SQLType[B](n, bc) {
      override def show(b : B) : String = parent.show(g(b))
      override def put(b : B) : Any = parent.put(g(b))
      def read(s : String) : Option[B] = parent.read(s).flatMap(f)
      override def get(x : Any, where : String) : B = f(parent.get(x, where))
        .getOrElse(throw new SQLTypeMismatch(x, this, parent.name).amend(where))
    }
}

object SQLType {
  def transform[A,B](name : String, cls : Class[B])(get : A => Option[B], put : B => A)(implicit base : SQLType[A]) : SQLType[B] =
    base.transform(name, cls)(get, put)
  def apply[A](name : String, cls : Class[A])(get : String => Option[A], put : A => String) : SQLType[A] =
    string.transform(name, cls)(get, put)

  implicit object string extends SQLType[String]("text", classOf[String]) {
    def read(s : String) = Some(s)
  }

  implicit object boolean extends SQLType[Boolean]("boolean", classOf[Boolean]) {
    def read(s : String) = s.toLowerCase match {
      case "t" => Some(true)
      case "f" => Some(false)
      case "true" => Some(true)
      case "false" => Some(false)
      case "y" => Some(true)
      case "n" => Some(false)
      case "yes" => Some(true)
      case "no" => Some(false)
      case "on" => Some(true)
      case "off" => Some(false)
      case "1" => Some(true)
      case "0" => Some(false)
      case _ => None
    }
    override def get(x : Any, where : String = "") : Boolean = x match {
      case null => throw new SQLUnexpectedNull(this, where)
      case b : Boolean => b
      case b : java.lang.Boolean => b
      case s : String => read(s).getOrElse(throw new SQLTypeMismatch(x, this, where))
      case _ => throw new SQLTypeMismatch(x, this, where)
    }
  }

  implicit object int extends SQLType[Int]("integer", classOf[Int]) {
    def read(s : String) = Maybe.toInt(s)
    override def get(x : Any, where : String = "") : Int = x match {
      case null => throw new SQLUnexpectedNull(this, where)
      case i : Int => i
      case i : java.lang.Integer => i
      case i : Short => i.toInt
      case i : java.lang.Short => i.toInt
      case s : String => read(s).getOrElse(throw new SQLTypeMismatch(x, this, where))
      case _ => throw new SQLTypeMismatch(x, this, where)
    }
  }

  implicit object long extends SQLType[Long]("bigint", classOf[Long]) {
    def read(s : String) = Maybe.toLong(s)
    override def get(x : Any, where : String = "") : Long = x match {
      case null => throw new SQLUnexpectedNull(this, where)
      case i : Long => i
      case i : java.lang.Long => i
      case s : String => read(s).getOrElse(throw new SQLTypeMismatch(x, this, where))
      case _ => throw new SQLTypeMismatch(x, this, where)
    }
  }

  implicit object date extends SQLType[Date]("date", classOf[Date]) {
    def read(s : String) =
      catching(classOf[java.lang.IllegalArgumentException]).opt(
        org.joda.time.LocalDate.parse(s))
  }

  implicit object timestamp extends SQLType[Timestamp]("timestamp", classOf[Timestamp]) {
    def read(s : String) =
      catching(classOf[java.lang.IllegalArgumentException]).opt(
        org.joda.time.DateTime.parse(s))
  }

  implicit object bigdecimal extends SQLType[BigDecimal]("numeric", classOf[BigDecimal]) {
    def read(s : String) = Maybe.toNumber(BigDecimal(s))
  }

  implicit def array[A](implicit t : SQLType[A]) : SQLType[IndexedSeq[A]] =
    new SQLType[IndexedSeq[A]](t.name + "[]", classOf[IndexedSeq[A]]) {
      /* TODO: */
      def show(a : A) : String = ???
      def put(a : A) : Any = ???
      def read(s : String) = None
      override def get(x : Any, where : String = "") : IndexedSeq[A] = x match {
        case null => throw new SQLUnexpectedNull(this, where)
        case a : IndexedSeq[Any] => a.map(t.get(_, where))
        case s : String => read(s).getOrElse(throw new SQLTypeMismatch(x, this, where))
        case _ => throw new SQLTypeMismatch(x, this, where)
      }
    }

  implicit def option[A](implicit t : SQLType[A]) : SQLType[Option[A]] =
    new SQLType[Option[A]](t.name, classOf[Option[A]]) {
      def read(s : String) : Option[Option[A]] = Option(s).map(t.read(_))
      override def get(x : Any, where : String = "") : Option[A] = Option(x).map(t.get(_, where))
      override def put(a : Option[A]) = a.map(t.put(_))//.orNull
    }

  def put[A](a : A)(implicit t : SQLType[A]) : Any = t.put(a)

  def get[A](a : Any)(implicit t : SQLType[A]) : A = t.get(a)
  def get[A](row : db.RowData, name : String)(implicit t : SQLType[A]) : A =
    t.get(row(name), "column " + name)
  def get[A](row : db.RowData, number : Int)(implicit t : SQLType[A]) : A =
    t.get(row(number), "column " + number)
}

case class Inet(ip : String)
object Inet {
  implicit val sqlType : SQLType[Inet] =
    SQLType[Inet]("inet", classOf[Inet])(s => Some(Inet(s)), _.ip)
}
