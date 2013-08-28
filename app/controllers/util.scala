package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation._
import util._

object form {
  def enumField(enum : Enumeration) = number(min=0, max=enum.maxId-1).transform[enum.Value](enum(_), _.id)
}

/* Useful for forms that have dynamically optional content (as opposed to user-optional) */
abstract sealed class MaybeMapping[T] extends Mapping[Option[T]]

case class NoMapping[T]() extends MaybeMapping[T] {
  val key = ""
  val mappings = Nil
  val constraints = Nil
  def bind(data : Map[String, String]) : Either[Seq[FormError], Option[T]] = Right(None)
  def unbind(value : Option[T]) : (Map[String, String], Seq[FormError]) = 
    Map.empty -> value.fold(Nil : Seq[FormError])(_ => Seq(FormError("", "non-empty NoMapping value")))
  def withPrefix(prefix : String) : Mapping[Option[T]] = this
  def verifying(constraints : Constraint[Option[T]]*) : Mapping[Option[T]] = this
}

case class SomeMapping[T](wrapped : Mapping[T]) extends MaybeMapping[T] {
  val key = wrapped.key
  val mappings = wrapped.mappings
  override val format = wrapped.format
  val constraints = wrapped.constraints.map { c =>
    Constraint[Option[T]](c.name, c.args)(_.fold(Invalid(ValidationError("error.required")) : ValidationResult)(c(_)))
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
  def apply[T](m : Option[Mapping[T]]) : MaybeMapping[T] = m.fold(NoMapping[T] : MaybeMapping[T])(SomeMapping[T](_))
}
