package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation._

object Field {
  def enum(enum : Enumeration, maxId : Option[Int] = None) = number(min=0, max=maxId.getOrElse(enum.maxId-1)).transform[enum.Value](enum(_), _.id)
}

object EmptyMapping extends Mapping[Unit] {
  val key = ""
  val mappings = Nil
  val constraints = Nil
  def bind(data : Map[String, String]) : Either[Seq[FormError], Unit] = Right(())
  def unbind(value : Unit) : (Map[String, String], Seq[FormError]) = (Map.empty, Nil)
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
  def unbind(value : Option[T]) : (Map[String, String], Seq[FormError]) = 
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
    value.fold(Map.empty[String,String] -> Seq(FormError(key, "empty SomeMapping value")))(wrapped.unbind(_))
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
final case class OptionMapping[T](wrapped : Mapping[T], val constraints: Seq[Constraint[Option[T]]] = Nil) extends Mapping[Option[T]] {
  val key = wrapped.key
  val mappings = wrapped.mappings
  override val format = wrapped.format
  def bind(data: Map[String, String]): Either[Seq[FormError], Option[T]] = {
    if (data.contains(key) || data.keys.exists(p => p.startsWith(key) && ".[".contains(p(key.length))))
      wrapped.bind(data).right.map(Some(_))
    else
      Right(None)
  }
  def unbind(value : Option[T]) = {
    val (m, e) = value.fold[(Map[String,String],Seq[FormError])](
      Map.empty -> Nil)(
      wrapped.unbind(_))
    (m, e ++ collectErrors(value))
  }
  def withPrefix(prefix : String) : Mapping[Option[T]] = 
    copy(wrapped = wrapped.withPrefix(prefix))
  def verifying(addConstraints : Constraint[Option[T]]*) : Mapping[Option[T]] =
    copy(constraints = constraints ++ addConstraints)
}
