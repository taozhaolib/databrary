package controllers
import scala.language.higherKinds

import play.api.mvc._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

/* This stuff is all temporary until https://github.com/playframework/playframework/pull/1986 */

/** A generic version of ActionBuilder parameterized over the request type.
  * Ideally, ActionBuilder[P] would extend ActionFunction[Request,P]. */
trait ActionFunction[-R[_],P[_]] {
  parent =>
  def invokeBlock[A](request : R[A], block : P[A] => Future[SimpleResult]) : Future[SimpleResult]

  /** Compose this ActionFunction with another. */
  def ~>[Q[_]](child : ActionFunction[P,Q]) : ActionFunction[R,Q] = new ActionFunction[R,Q] {
    def invokeBlock[A](request : R[A], block : Q[A] => Future[SimpleResult]) =
      parent.invokeBlock(request, (p : P[A]) => child.invokeBlock(p, block))
  }

  // def simple(r : SimpleResult) : Future[SimpleResult] = Future.successful(r)
}

trait ActionRefiner[-R[_],P[_]] extends ActionFunction[R,P] {
  protected def refine[A](request : R[A]) : Future[Either[SimpleResult,P[A]]]
  final def invokeBlock[A](request : R[A], block : P[A] => Future[SimpleResult]) =
    refine(request).flatMap(_.fold(Future.successful _, block))
}

trait ActionTransformer[-R[_],P[_]] extends ActionRefiner[R,P] {
  protected def transform[A](request : R[A]) : Future[P[A]]
  final protected def refine[A](request : R[A]) =
    transform(request).map(Right(_))
}

trait ActionHandler[R[_]] extends ActionRefiner[R,R] {
  protected def handle[A](request : R[A]) : Future[Option[SimpleResult]]
  final protected def refine[A](request : R[A]) =
    handle(request).map(_.toLeft(request))
}

trait ActionChecker[R[_]] extends ActionHandler[R] {
  protected def check[A](request : R[A]) : Future[Unit]
  final protected def handle[A](request : R[A]) =
    check(request).map(_ => None)
}

trait ActionCreator[P[_]] extends ActionBuilder[P] with ActionFunction[Request,P] {
  parent =>
  override def ~>[Q[_]](child : ActionFunction[P,Q]) : ActionCreator[Q] = new ActionCreator[Q] {
    def invokeBlock[A](request : Request[A], block : Q[A] => Future[SimpleResult]) =
      parent.invokeBlock(request, (p : P[A]) => child.invokeBlock(p, block))
  }
}
