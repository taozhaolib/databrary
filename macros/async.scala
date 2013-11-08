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

  /** Fold results of the futures over the operation, serially, in arbitrary order. */
  def fold[A](l : GenTraversableOnce[Future[A]], z : A = ())(op : (A, A) => A = ((_ : Unit, _ : Unit) => ()))(implicit context : ExecutionContext) : Future[A] =
    l.fold(successful(z)) { (a, b) =>
      a.flatMap(a => b.map(op(a, _)))
    }
  /** Fold results of the futures over the operation, serially left-to-right. */
  def foldLeft[A,B](l : GenTraversableOnce[Future[A]], z : B)(op : (B, A) => B)(implicit context : ExecutionContext) : Future[B] =
    l.foldLeft(successful(z)) { (b, a) =>
      b.flatMap(b => a.map(op(b, _)))
    }
  /** Evaluate each of the futures, serially left-to-right, and produce a list of the results. */
  def sequence[A, L[X] <: GenTraversableOnce[X], R](l : L[Future[A]])(implicit bf : generic.CanBuildFrom[L[Future[A]], A, R], context : ExecutionContext) : Future[R] = {
    val b = bf()
    l.foldLeft[Future[Any]](successful(())) { (r, a) =>
      r.flatMap(_ => a.andThen { case Success(a) => b += a })
    }.map(_ => b.result)
  }
  /** Evaluate each of the futures in the Map in an arbitrary order and produce a collection of the results.
    * This is not as efficient as it could be due to a lack of foldMap/mapAccum-type functions. */
  def sequenceValues[K, A, R](m : Map[K, Future[A]])(implicit bf : generic.CanBuildFrom[Map[K, Future[A]], (K, A), R], context : ExecutionContext) : Future[R] = {
    val b = bf()
    def madd(x : AnyRef) : Future[Any] = x match {
      case f : Future[Any] => f
      case (k : K, a : Future[A]) => a.map(v => b.+=((k, v)))
    }
    madd(m.fold[AnyRef](successful(())) { (l, r) =>
      madd(l).flatMap(_ => madd(r))
    }).map(_ => b.result)
  }

  /** Evaluate each of the futures, serially left-to-right, and produce a list of the results. */

  /** Unsafely retrieve the value of an already evaluated Future. */
  def get[A](a : Future[A]) : A = a.value.get.get
}
