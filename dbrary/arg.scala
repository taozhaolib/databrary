package dbrary.SQL

import macros._

class Arg[A](val value : A)(implicit val sqlType : Type[A]) extends Statement {
  final def put : Any = sqlType.put(value)
  final def escaped = sqlType.escaped(value)

  def statement = "?::" + sqlType.name
  final def args = Seq(this)
  override def toLiteral = new LiteralStatement(escaped)

  def ++(s : Statement) = new PreparedStatement(statement + s.statement, this +: s.args)
  def ++:(s : Statement) = new PreparedStatement(s.statement + statement, s.args :+ this)
  def +(s : String) : Statement = new PreparedStatement(statement + s, args)
  def +:(s : String) : Statement = new PreparedStatement(s + statement, args)
}

object Arg {
  def apply[A](value : A)(implicit sqlType : Type[A]) = new Arg[A](value)(sqlType)
}

/** A list of arguments that may be passed to a query. */
class Args(val args : Seq[Arg[_]]) extends Iterable[Arg[_]] {
  final def iterator = args.iterator
  def ++(other : Iterable[Arg[_]]) : Args = new Args(args ++ other)
  def :+(other : Arg[_]) : Args = new Args(args :+ other)
  def +:(other : Arg[_]) : Args = new Args(other +: args)
  def :+[A : Type](other : A) : Args = new Args(args :+ Arg(other))
  def +:[A : Type](other : A) : Args = new Args(Arg(other) +: args)
  final def *(n : Int) : Args = new Args(0.until(n).flatMap(_ => args))
  final def join(sep : String = ",") = Statement.join(sep, args : _*)
}

/** A special case of Args for running simple queries with no arguments. */
object NoArgs extends Args(Nil)

/** Generic trait for anything which may accept Type args to produce a result. */
abstract trait ArgsView[+R] extends RepeatedView[Type, Arg[_], R] {
  protected final def arg[A : Type](a : A) = Arg(a)
  final def apply(a : Args) : R = result(a.args : _*)
}

object Args extends ArgsView[Args] {
  protected def result(args : Arg[_]*) : Args = new Args(args)
}
