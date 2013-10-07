package controllers
import scala.language.higherKinds

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation._
import scala.concurrent.Future
import util._
import dbrary.Offset

trait ActionFunction[-R[_],P[_]] {
  parent =>
  def invokeBlock[A](request : R[A], block : P[A] => Future[SimpleResult]) : Future[SimpleResult]

  def ~>[Q[_]](child : ActionFunction[P,Q]) : ActionFunction[R,Q] = new ActionFunction[R,Q] {
    def invokeBlock[A](request : R[A], block : Q[A] => Future[SimpleResult]) =
      parent.invokeBlock(request, (p : P[A]) => child.invokeBlock(p, block))
  }

  // def simple(r : SimpleResult) : Future[SimpleResult] = Future.successful(r)
}

trait ActionRefiner[-R[_],P[_]] extends ActionFunction[R,P] {
  protected def refine[A](request : R[A]) : Either[Future[SimpleResult],P[A]]
  final def invokeBlock[A](request : R[A], block : P[A] => Future[SimpleResult]) =
    refine(request).fold(identity, block)
  def simple[A](r : SimpleResult) : Either[Future[SimpleResult],P[A]] = Left(Future.successful(r))
}

trait ActionHandler[R[_]] extends ActionRefiner[R,R] {
  protected def handle[A](request : R[A]) : Option[Future[SimpleResult]]
  final protected def refine[A](request : R[A]) = handle(request).toLeft(request)
  def simple(r : SimpleResult) : Option[Future[SimpleResult]] = Some(Future.successful(r))
}

trait ActionCreator[P[_]] extends ActionBuilder[P] with ActionFunction[Request,P] {
  parent =>
  override def ~>[Q[_]](child : ActionFunction[P,Q]) : ActionCreator[Q] = new ActionCreator[Q] {
    def invokeBlock[A](request : Request[A], block : Q[A] => Future[SimpleResult]) =
      parent.invokeBlock(request, (p : P[A]) => child.invokeBlock(p, block))
  }
}

object Field {
  def enum(enum : Enumeration) = number(min=0, max=enum.maxId-1).transform[enum.Value](enum(_), _.id)
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
