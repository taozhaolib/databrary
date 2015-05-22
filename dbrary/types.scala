package dbrary.SQL

import scala.util.control.Exception.catching
import com.github.mauricio.async.db
import play.api.libs.json.{JsValue,Json}
import macros._
import dbrary._

class TypeMismatch(value : Any, sqltype : Type[_], where : String = "")
  extends db.exceptions.DatabaseException("Type mismatch converting " + (if (value == null) "null" else value.toString + ":" + value.getClass) + " to " + sqltype.name + Maybe.bracket(" for ", where)) {
  def amend(msg : String) : TypeMismatch = new TypeMismatch(value, sqltype, Maybe.bracket("", where, " in ") + msg)
}

class UnexpectedNull(sqltype : Type[_], where : String = "") extends TypeMismatch(null, sqltype, where) {
  override def amend(msg : String) : UnexpectedNull = new UnexpectedNull(sqltype, Maybe.bracket("", where, " in ") + msg)
}

abstract class Type[A](val name : String, val aClass : Class[A]) {
  parent =>
  def show(a : A) : String
  def put(a : A) : Any = a
  def read(s : String) : Option[A]
  def get(x : Any, where : String = "") : A = x match {
    case null => throw new UnexpectedNull(this, where)
    case a /* : A */ if aClass.isAssignableFrom(a.getClass) => a.asInstanceOf[A]
    case s : String => read(s).getOrElse(throw new TypeMismatch(x, this, where))
    case _ => throw new TypeMismatch(x, this, where)
  }
  def escaped(a : A) : String = quoted(show(a))

  final def transform[B](n : String, bc : Class[B])(f : A => Option[B], g : B => A) : Type[B] =
    new Type[B](n, bc) {
      def show(b : B) : String = parent.show(g(b))
      override def put(b : B) : Any = parent.put(g(b))
      def read(s : String) : Option[B] = parent.read(s).flatMap(f)
      override def get(x : Any, where : String) : B = f(parent.get(x, where))
        .getOrElse(throw new TypeMismatch(x, this, parent.name).amend(where))
      override def escaped(b : B) : String = parent.escaped(g(b))
    }
}

abstract class DBType[A](name : String, aClass : Class[A], column : db.column.ColumnEncoderDecoder) extends Type[A](name, aClass) {
  def show(a : A) : String = column.encode(a)
  def read(s : String) : Option[A] = Some(column.decode(s).asInstanceOf[A])
}

object Type {
  def transform[A,B](name : String, cls : Class[B])(get : A => Option[B], put : B => A)(implicit base : Type[A]) : Type[B] =
    base.transform(name, cls)(get, put)
  def apply[A](name : String, cls : Class[A])(get : String => Option[A], put : A => String) : Type[A] =
    string.transform(name, cls)(get, put)

  implicit object string extends DBType[String]("text", classOf[String], db.column.StringEncoderDecoder)

  implicit object boolean extends DBType[Boolean]("boolean", classOf[Boolean], db.postgresql.column.BooleanEncoderDecoder) {
    override def read(s : String) = s.toLowerCase match {
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
      case null => throw new UnexpectedNull(this, where)
      case b : Boolean => b
      case b : java.lang.Boolean => b
      case s : String => read(s).getOrElse(throw new TypeMismatch(x, this, where))
      case _ => throw new TypeMismatch(x, this, where)
    }
    override def escaped(a : Boolean) = a.toString
  }

  implicit object short extends DBType[Short]("smallint", classOf[Short], db.column.ShortEncoderDecoder) {
    override def get(x : Any, where : String = "") : Short = x match {
      case null => throw new UnexpectedNull(this, where)
      case i : Short => i
      case i : java.lang.Short => i
      case s : String => read(s).getOrElse(throw new TypeMismatch(x, this, where))
      case _ => throw new TypeMismatch(x, this, where)
    }
    override def escaped(a : Short) = show(a)
  }

  implicit object int extends DBType[Int]("integer", classOf[Int], db.column.IntegerEncoderDecoder) {
    override def get(x : Any, where : String = "") : Int = x match {
      case null => throw new UnexpectedNull(this, where)
      case i : Int => i
      case i : java.lang.Integer => i
      case i : Short => i.toInt
      case i : java.lang.Short => i.toInt
      case s : String => read(s).getOrElse(throw new TypeMismatch(x, this, where))
      case _ => throw new TypeMismatch(x, this, where)
    }
    override def escaped(a : Int) = show(a)
  }

  implicit object long extends DBType[Long]("bigint", classOf[Long], db.column.LongEncoderDecoder) {
    override def get(x : Any, where : String = "") : Long = x match {
      case null => throw new UnexpectedNull(this, where)
      case i : Long => i
      case i : java.lang.Long => i
      case s : String => read(s).getOrElse(throw new TypeMismatch(x, this, where))
      case _ => throw new TypeMismatch(x, this, where)
    }
    override def escaped(a : Long) = show(a)
  }

  implicit object date extends DBType[Date]("date", classOf[Date], db.column.DateEncoderDecoder)

  implicit object timestamp extends Type[Timestamp]("timestamp", classOf[Timestamp]) {
    def show(t : Timestamp) = t.toString
    def read(s : String) =
      catching(classOf[java.lang.IllegalArgumentException]).opt(
        org.joda.time.LocalDateTime.parse(s))
  }

  implicit object interval extends DBType[org.joda.time.Period]("interval", classOf[org.joda.time.Period], db.postgresql.column.PostgreSQLIntervalEncoderDecoder)

  implicit object numeric extends DBType[BigDecimal]("numeric", classOf[BigDecimal], db.column.BigDecimalEncoderDecoder)

  implicit object bytea extends DBType[Array[Byte]]("bytea", classOf[Array[Byte]], db.postgresql.column.ByteArrayEncoderDecoder)

  implicit val url : Type[java.net.URL] =
    string.transform("text", classOf[java.net.URL])(dbrary.url.parse, _.toString)

  /* can be generalized to any traversable/array (but read as IndexedSeq)
   * only works for 1-dimentional arrays */
  final class array[A](implicit t : Type[A]) extends Type[IndexedSeq[A]](t.name + "[]", classOf[IndexedSeq[A]]) {
    private[this] def quote(s : String) =
      if (s == null) "null"
      else "\"" + s.replaceAllLiterally("\\", """\\""").replaceAllLiterally("\"", """\"""") + "\""
    private[this] class delegate extends db.postgresql.util.ArrayStreamingParserDelegate {
      private val builder = IndexedSeq.newBuilder[A]
      private var failed = false
      def result = if (failed) None else Some(builder.result)

      override def elementFound(element : String) {
        t.read(element) match {
          case Some(x) => builder += x
          case None => failed = true
        }
      }
      override def nullElementFound {
        elementFound(null)
      }
    }
    def show(a : IndexedSeq[A]) : String = a.map(a => quote(t.show(a))).mkString("{", ",", "}")
    override def put(a : IndexedSeq[A]) : Any = a.map(t.put)
    def read(s : String) = {
      val d = new delegate
      db.postgresql.util.ArrayStreamingParser.parse(s, d)
      d.result
    }
    override def get(x : Any, where : String = "") : IndexedSeq[A] = x match {
      case null => throw new UnexpectedNull(this, where)
      case a : IndexedSeq[Any] => a.map(t.get(_, where))
      case s : String => read(s).getOrElse(throw new TypeMismatch(x, this, where))
      case _ => throw new TypeMismatch(x, this, where)
    }
  }
  implicit def array[A : Type] : Type[IndexedSeq[A]] = new array[A]

  final class option[A](implicit t : Type[A]) extends Type[Option[A]](t.name, classOf[Option[A]]) {
    def show(a : Option[A]) : String = a.fold[String](null)(t.show)
    def read(s : String) : Option[Option[A]] =
      if (s == null) Some(None)
      else t.read(s).map(Some(_))
    override def get(x : Any, where : String = "") : Option[A] = Option(x).map(t.get(_, where))
    override def put(a : Option[A]) = a.map(t.put)//.orNull
    override def escaped(a : Option[A]) : String = a.fold("null")(t.escaped)
  }
  implicit def option[A : Type] : Type[Option[A]] = new option[A]

  final class default[A](default : A)(implicit t : Type[A]) extends Type[A](t.name, t.aClass) {
    def show(a : A) = t.show(a)
    def read(s : String) : Option[A] = Option(s).fold[Option[A]](Some(default))(t.read)
    override def put(a : A) = t.put(a)
    override def get(x : Any, where : String = "") : A = Option(x).fold(default)(t.get(_, where))
    override def escaped(a : A) = t.escaped(a)
  }

  implicit object jsonValue extends Type[JsValue]("json", classOf[JsValue]) {
    def show(j : JsValue) : String = j.toString
    override def put(j : JsValue) : Any = j.toString
    def read(s : String) : Option[JsValue] =
      catching(classOf[com.fasterxml.jackson.core.JsonProcessingException]).opt(Json.parse(s))
  }

  def put[A](a : A)(implicit t : Type[A]) : Any = t.put(a)

  def get[A](a : Any)(implicit t : Type[A]) : A = t.get(a)
  def get[A](row : db.RowData, name : String)(implicit t : Type[A]) : A =
    t.get(row(name), "column " + name)
  def get[A](row : db.RowData, number : Int)(implicit t : Type[A]) : A =
    t.get(row(number), "column " + number)
}
