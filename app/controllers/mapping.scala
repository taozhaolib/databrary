package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation._
import macros._

object Mappings {
  val raw = of[Option[String]](new format.Formatter[Option[String]] {
    def bind(key: String, data: Map[String, String]) = Right(data.get(key))
    def unbind(key: String, value: Option[String]) = value.fold(Map.empty[String, String])(v => Map(key -> v))
  })
  def option[A](map : Mapping[A]) : OptionMapping[A] = OptionMapping[A](map)
  def some[A](map : Mapping[A], default : A = "") : Mapping[Option[A]] =
    map.transform[Option[A]](Some(_), _.getOrElse(default))
  def enum(enum : Enumeration, maxId : Option[Int] = None, minId : Int = 0) =
    number(min=minId, max=maxId.getOrElse(enum.maxId-1)).transform[enum.Value](enum(_), _.id)
  val text : Mapping[String] = Forms.text.transform[String](_.trim, identity)
  val nonEmptyText : Mapping[String] = text verifying Constraints.nonEmpty
  val maybeText : Mapping[Option[String]] =
    Forms.optional(Forms.text).transform[Option[String]](_.flatMap(s => Maybe(s.trim).opt()), identity)
  import models.{AbstractTag,TagName}
  val tag : Mapping[AbstractTag] = Forms.text
    .verifying("tag.invalid", t => TagName.validate(t).nonEmpty)
    .transform[AbstractTag](TagName(_), _.name)

  private def arrayLength(length : Int) : Constraint[Array[_]] =
    Constraint[Array[_]]("constraint.length", length) { a =>
      if (a.length != length) Invalid(ValidationError(if (a.length > length) "error.maxLength" else "error.minLength", length))
      else Valid
    }
  def encoded(encoding : store.Encoding) : Mapping[Array[Byte]] =
    FieldMapping[Array[Byte]]()(encoding.formatter)
  def hash(hash : store.Hash, encoding : store.Encoding = store.Hex) : Mapping[Array[Byte]] =
    encoded(encoding) verifying arrayLength(hash.size)
}

object EmptyMapping extends Mapping[Unit] {
  val key = ""
  val mappings = Nil
  val constraints = Nil
  def bind(data : Map[String, String]) : Either[Seq[FormError], Unit] = Right(())
  def unbind(value : Unit) : Map[String, String] = Map.empty
  def unbindAndValidate(value : Unit) : (Map[String, String], Seq[FormError]) = (Map.empty, Nil)
  def withPrefix(prefix : String) : Mapping[Unit] = this
  def verifying(constraints : Constraint[Unit]*) : Mapping[Unit] = this
}

/* Useful for forms that have dynamically optional content (as opposed to user-optional) */
abstract sealed class MaybeMapping[T] extends Mapping[Option[T]]

final case class NoMapping[T]() extends MaybeMapping[T] {
  val key = ""
  val mappings = Nil
  val constraints = Nil
  def bind(data : Map[String, String]) : Either[Seq[FormError], Option[T]] = Right(None)
  def unbind(value : Option[T]) : Map[String, String] = Map.empty
  def unbindAndValidate(value : Option[T]) : (Map[String, String], Seq[FormError]) =
    Map.empty -> value.fold[Seq[FormError]](Nil)(_ => Seq(FormError("", "non-empty NoMapping value")))
  def withPrefix(prefix : String) : Mapping[Option[T]] = this
  def verifying(constraints : Constraint[Option[T]]*) : Mapping[Option[T]] = this
}

final case class SomeMapping[T](wrapped : Mapping[T]) extends MaybeMapping[T] {
  val key = wrapped.key
  val mappings = wrapped.mappings
  override val format = wrapped.format
  val constraints = wrapped.constraints.map { c =>
    Constraint[Option[T]](c.name, c.args)(_.fold[ValidationResult](Invalid(ValidationError("error.required")))(c(_)))
  }
  def bind(data : Map[String, String]) : Either[Seq[FormError], Option[T]] =
    wrapped.bind(data).right.map(Some(_))
  def unbind(value : Option[T]) =
    value.fold(Map.empty[String,String])(wrapped.unbind(_))
  def unbindAndValidate(value : Option[T]) =
    value.fold(Map.empty[String,String] -> Seq(FormError(key, "empty SomeMapping value")))(wrapped.unbindAndValidate(_))
  def withPrefix(prefix : String) : Mapping[Option[T]] =
    SomeMapping[T](wrapped.withPrefix(prefix))
  def verifying(constraints : Constraint[Option[T]]*) : Mapping[Option[T]] =
    SomeMapping[T](wrapped.verifying(constraints.map { c =>
      Constraint[T](c.name, c.args)(a => c(Some(a)))
    } : _*))
}

object MaybeMapping {
  def apply[T](m : Option[Mapping[T]]) : MaybeMapping[T] = m.fold[MaybeMapping[T]](NoMapping[T])(SomeMapping[T](_))
}


/** Identical to OptionalMapping except that empty fields are not treated as missing. */
final case class OptionMapping[T](wrapped : Mapping[T], val constraints : Seq[Constraint[Option[T]]] = Nil) extends Mapping[Option[T]] {
  val key = wrapped.key
  val mappings = wrapped.mappings
  override val format = wrapped.format
  def bind(data: Map[String, String]) : Either[Seq[FormError], Option[T]] = {
    if (data.contains(key) || data.keys.exists(p => p.startsWith(key) && ".[".contains(p(key.length))))
      wrapped.bind(data).right.map(Some(_))
    else
      Right(None)
  }
  def unbind(value : Option[T]) =
    value.fold[Map[String,String]](Map.empty)(wrapped.unbind(_))
  def unbindAndValidate(value : Option[T]) = {
    val (m, e) = value.fold[(Map[String,String],Seq[FormError])](
      Map.empty -> Nil)(
      wrapped.unbindAndValidate(_))
    (m, e ++ collectErrors(value))
  }
  def withPrefix(prefix : String) : Mapping[Option[T]] =
    copy(wrapped = wrapped.withPrefix(prefix))
  def verifying(addConstraints : Constraint[Option[T]]*) : Mapping[Option[T]] =
    copy(constraints = constraints ++ addConstraints)
}

final case class EitherMapping[L,R](leftMapping : Mapping[L], rightMapping : Mapping[R], val key : String = "", val constraints : Seq[Constraint[Either[L,R]]] = Nil) extends Mapping[Either[L,R]] {
  private val left = leftMapping.withPrefix(key)
  private val right = rightMapping.withPrefix(key)
  val mappings = left.mappings ++ right.mappings
  def bind(data: Map[String, String]) : Either[Seq[FormError], Either[L,R]] = {
    val l = left.bind(data)
    val r = right.bind(data)
    l.fold(e => r.fold(er => Left(e ++ er),
        r => Right(Right(r))),
      l => r.fold(_ => Right(Left(l)),
        _ => Left(Seq(FormError(key, "error.both", Seq(left.key, right.key))))))
  }
  def unbind(value : Either[L,R]) =
    value.fold(left.unbind(_), right.unbind(_))
  def unbindAndValidate(value : Either[L,R]) =
    value.fold(left.unbindAndValidate(_), right.unbindAndValidate(_))
  def withPrefix(prefix : String) : Mapping[Either[L,R]] =
    addPrefix(prefix).fold(this)(k => copy(key = k))
  def verifying(addConstraints : Constraint[Either[L,R]]*) : Mapping[Either[L,R]] =
    copy(constraints = constraints ++ addConstraints)
}

final case class KeyedMapping[T](wrapped : Mapping[T], val key : String = "", val constraints : Seq[Constraint[Map[String, T]]] = Nil) extends Mapping[Map[String, T]] {
  private val keyDot = if (key.nonEmpty) key + "." else ""
  override val format = wrapped.format
  val mappings = wrapped.mappings
  def bind(data : Map[String, String]) : Either[Seq[FormError], Map[String, T]] = {
    val Field = ("^(" + java.util.regex.Pattern.quote(keyDot) + "([^.\\[]*))").r
    val r = data.keys.collect { case Field(k, f) => (f, k) }.toMap
      .mapValues(k => wrapped.withPrefix(k).bind(data))
    val err = r.values.collect { case Left(e) => e }
    if (err.nonEmpty)
      Left(err.toSeq.flatten)
    else
      applyConstraints(r.mapValues(_.right.get))
  }
  def unbind(value : Map[String, T]) =
    value.foldLeft(Map.empty[String, String]) { case (r, (f, v)) =>
      r ++ wrapped.withPrefix(keyDot + f).unbind(v)
    }
  def unbindAndValidate(value : Map[String, T]) =
    value.foldLeft((Map.empty[String, String], collectErrors(value))) { case ((r, e), (f, v)) =>
      val (wr, we) = wrapped.withPrefix(keyDot + f).unbindAndValidate(v)
      (r ++ wr, e ++ we)
    }
  def withPrefix(prefix : String) =
    addPrefix(prefix).fold(this)(k => this.copy(key = k))
  def verifying(addConstraints : Constraint[Map[String, T]]*) : Mapping[Map[String, T]] =
    copy(constraints = constraints ++ addConstraints)
}
