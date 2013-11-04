package macros

import scala.concurrent.{Future,ExecutionContext}
import Future.successful

object Async {
  /** Shorter alias for Future.successful. */
  def apply[A](a : A) : Future[A] = successful(a)

  private[this] def ss[A](a : A) : Future[Option[A]] = successful(Some(a))
  /** Unwrap and map an Option into a Future Option. */
  def flatMap[A,B](a : Option[A], f : A => Future[Option[B]]) : Future[Option[B]] =
    a.fold[Future[Option[B]]](successful(None))(f(_))
  /** Unwrap and map an Option into a Future. */
  def map[A,B](a : Option[A], f : A => Future[B])(implicit context : ExecutionContext) : Future[Option[B]] =
    flatMap[A,B](a, f(_).map(Some(_)))
  def orElse[A](a : Option[A], b : => Future[Option[A]]) : Future[Option[A]] =
    a.fold(b)(ss _)

  /** Unsafely retrieve the value of an already evaluated Future. */
  def get[A](a : Future[A]) : A = a.value.get.get
}
