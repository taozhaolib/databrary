package object macros {
  import scala.collection._
  import scala.language.experimental.macros

  /** Cast Any to Option[A].  Equivalent to: `cast[A](x) = x match { a : A => Some(a) ; _ => None }` */
  def cast[A](x : Any) : Option[A] = macro Cast.castImpl[A]

  /** What a.zip(b) should do but doesn't?
    * Takes an optional function to map the result over. */
  def zip[A,B,C](a : Option[A], b : Option[B], f : (A, B) => C = Tuple2.apply _) : Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  def partition[T <: Traversable[A],A,B,C,BR,CR](l : T, f : PartialFunction[A, Either[B,C]])(implicit bf : generic.CanBuildFrom[T, B, BR], cf : generic.CanBuildFrom[T, C, CR]) : (BR,CR) = {
    val br = bf()
    val cr = cf()
    for (x <- l) f(x).fold(br += _, cr += _)
    (br.result, cr.result)
  }

  /** Group adjacent elements with identical keys into nested lists, such that the concatenation of the resulting _2 elements is the original list.
    * This is more like Haskell's group than scala's Seq.groupBy: only adjacent elements are grouped.
    * @param l the list to group
    * @param f the key-generating function to group by
    */
  def groupBy[T <: TraversableLike[A,T],A,K,R](l : T, f : A => K)(implicit bf : generic.CanBuildFrom[T, (K,T), R]) : R = {
    val r = bf()
    @scala.annotation.tailrec def next(l : T) : Unit = if (l.nonEmpty) {
      val k = f(l.head)
      val (p, s) = l.span(f(_).equals(k))
      r += k -> p
      next(s)
    }
    next(l)
    r.result
  }

  /** Apply a function to the first component of a tuple. */
  def first[A,B,C](a : (A, B), f : A => C) : (C, B) =
    (f(a._1), a._2)
  /** Apply a function to the first component of a tuple. */
  def second[A,B,C](a : (A, B), f : B => C) : (A, C) =
    (a._1, f(a._2))
  /** Apply a function to both components of a homogenous tuple. */
  def both[A,B](a : (A, A), f : A => B) : (B, B) =
    (f(a._1), f(a._2))

  def unwords(s : String*) = s.mkString(" ")

  def const[A](x : A) : Any => A = _ => x

  def apply[A,B](f : A => B, a : A) : B = f(a)
  def tupleApply[A,B](fa : (A => B, A)) : B = fa._1(fa._2)

  def max[A](x : A, y : A)(implicit o : Ordering[A]) = o.max(x, y)
  def min[A](x : A, y : A)(implicit o : Ordering[A]) = o.min(x, y)

  implicit final class AnyOps[A](a : A) {
    def |>[B](f : A => B) : B = f(a)
  }

  implicit final class TryOps[A](t : scala.util.Try[A]) {
    import scala.util.{Success,Failure}
    def toEither : Either[Throwable, A] = t match {
      case Success(a) => Right(a)
      case Failure(e) => Left(e)
    }

    import scala.concurrent.Future
    def toFuture : Future[A] = t match {
      case Success(a) => Future.successful(a)
      case Failure(e) => Future.failed(e)
    }
  }
}

package macros {
  case class Orderings[A](l : Ordering[A]*) extends Ordering[A] {
    def compare(x : A, y : A) : Int = {
      for (o <- l) {
        val r = o.compare(x, y)
        if (r != 0)
          return r
      }
      0
    }
  }
}
