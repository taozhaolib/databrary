package controllers
import scala.language.higherKinds

import play.api.mvc._
import scala.concurrent.Future

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
