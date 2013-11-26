package macros

import scala.concurrent.{Future,ExecutionContext}
import Future.successful
import scala.collection.{GenTraversableOnce,generic}
import scala.util.Success
import scala.language.higherKinds

/** Various utilities for dealing with Futures.
  * All operations are serial (as opposed to their counterparts on Future, which tend to be parallel.
  */
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
  def getOrElse[A](a : Option[A], b : => Future[A]) : Future[A] =
    a.fold(b)(successful _)
  def filter[A](a : Option[A], f : A => Future[Boolean])(implicit context : ExecutionContext) : Future[Option[A]] =
    flatMap[A,A](a, a => f(a).map { case false => None ; case true => Some(a) })

  /** Evaluate each of the futures, serially. */
  def foreach[A, R](l : Seq[A], f : A => Future[_], r : => R = ())(implicit context : ExecutionContext) : Future[R] = {
    l.foldLeft[Future[Any]](successful(())) { (r, a) =>
      r.flatMap(_ => f(a))
    }.map(_ => r)
  }
  /** Evaluate each of the futures, serially left-to-right, and produce a list of the results. */
  def map[A, B, R](l : Seq[A], f : A => Future[B])(implicit bf : generic.CanBuildFrom[Seq[A], B, R], context : ExecutionContext) : Future[R] = {
    val b = bf()
    foreach[A, R](l, f(_).andThen { case Success(a) => b += a }, b.result)
  }
  /** Evaluate each of the futures in the Map in an arbitrary order and produce a collection of the results.
    * This is not as efficient as it could be due to a lack of foldMap/mapAccum-type functions. */
  def mapValues[K, A, B, R](m : Map[K, A], f : A => Future[B])(implicit bf : generic.CanBuildFrom[Map[K, A], (K, B), R], context : ExecutionContext) : Future[R] = {
    val b = bf()
    def madd(x : AnyRef) : Future[Any] = x match {
      case f : Future[Any] => f
      case (k : K, a : A) => f(a).map(v => b.+=((k, v)))
    }
    madd(m.fold[AnyRef](successful(())) { (l, r) =>
      madd(l).flatMap(_ => madd(r))
    }).map(_ => b.result)
  }

  def peek[A](a : Future[A]) : Option[A] = a.value.map(_.get)
  /** Unsafely retrieve the value of an already evaluated Future. */
  private final class UnevaluatedFutureException extends RuntimeException("Future has not yet completed")
  def get[A](a : Future[A]) : A = a.value match {
    case Some(v) => v.get
    case None => throw new UnevaluatedFutureException /* checked explicitly to shorten stack trace */
  }
  @deprecated("blocking call", "") def wait[A](a : Future[A]) : A =
    scala.concurrent.Await.result(a, scala.concurrent.duration.Duration(1, scala.concurrent.duration.SECONDS))
}
