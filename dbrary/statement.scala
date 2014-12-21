package dbrary.SQL

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db

/** A string SQL statement and associated (placeholder) argments. */
trait Statement {
  def statement : String
  def args : Seq[Arg[_]]

  def toPrepared : PreparedStatement = PreparedStatement(statement, args)
  def toLiteral : LiteralStatement = LiteralStatement(statement, args)
  override def toString : String = {
    val a = args
    (if (a.nonEmpty) "{" + a.length + "}" else "") + toLiteral.toString
  }
  
  def ++(s : Statement) : Statement
  def ++:(s : Statement) : Statement
  def +(s : String) : Statement
  def +:(s : String) : Statement
  final def :?[A : Type](a : A) : Statement = this.++(new Arg[A](a))
  final def ?:[A : Type](a : A) : Statement = this.++:(new Arg[A](a))
}

class StatementProxy(val self : Statement) extends Statement with Proxy {
  final def statement = self.statement
  final def args = self.args

  def ++(s : Statement) = self.++(s)
  def ++:(s : Statement) = self.++:(s)
  def +(s : String) = self.+(s)
  def +:(s : String) = self.+:(s)
}

class SimpleStatement(val statement : String) extends Statement {
  final def args = Nil
  override def toString = statement

  def ++(s : Statement) = Statement(statement + s.statement, s.args)
  def ++:(s : Statement) = Statement(s.statement + statement, s.args)
  def +(s : String) = new SimpleStatement(statement + s)
  def +:(s : String) = new SimpleStatement(s + statement)
}

object EmptyStatement extends SimpleStatement("")

trait Query extends Statement {
  val statement : String
  def ++(s : Statement) : Query
  def ++:(s : Statement) : Query
  def +(s : String) : Query
  def +:(s : String) : Query
  final def join(delim : String, s : Statement*) =
    if (s.isEmpty) this else this + delim ++ Statement.join(delim, s : _*)

  protected final def send(implicit dbc : db.Connection, context : ExecutionContext) : Future[db.QueryResult] = {
    val s = statement
    val a = args
    val r = if (a.isEmpty)
      dbc.sendQuery(s)
    else
      dbc.sendPreparedStatement(s, a.map(_.put))
    if (logger.isTraceEnabled) {
      val t0 = System.nanoTime
      r.onComplete { r =>
        logger.trace(((System.nanoTime - t0) / 1e9).formatted("%8.5f: ") + this)
      }
    }
    r
  }

  def execute(implicit dbc : db.Connection, context : ExecutionContext) : Future[Unit] =
    send(dbc, context).map(_ => ())
  def run(implicit dbc : db.Connection, context : ExecutionContext) : Result =
    new Result(send(dbc, context))
  def run[A](parse : Row[A])(implicit dbc : db.Connection, context : ExecutionContext) : Rows[A] =
    new Rows(send(dbc, context), parse)
}

final class PreparedStatement(val statement : String, val args : Seq[Arg[_]]) extends Query {
  override def toPrepared = this
  override def ++(s : Statement) = new PreparedStatement(statement + s.statement, args ++ s.args)
  override def ++:(s : Statement) = new PreparedStatement(s.statement + statement, s.args ++: args)
  override def +(s : String) = new PreparedStatement(statement + s, args)
  override def +:(s : String) = new PreparedStatement(s + statement, args)
}

object PreparedStatement {
  def apply(statement : String) : PreparedStatement =
    new PreparedStatement(statement, Nil)
  def apply(statement : String, args : Seq[Arg[_]]) : PreparedStatement =
    new PreparedStatement(statement, args)
}

final class LiteralStatement(statement : String) extends SimpleStatement(statement) with Query {
  override def toLiteral = this
  override def ++(s : Statement) = new LiteralStatement(statement + s.toLiteral.statement)
  override def ++:(s : Statement) = new LiteralStatement(s.toLiteral.statement + statement)
  override def +(s : String) = new LiteralStatement(statement + s)
  override def +:(s : String) = new LiteralStatement(s + statement)
}

object LiteralStatement {
  def apply(statement : String) : LiteralStatement =
    new LiteralStatement(statement)
  def apply(statement : String, args : Seq[Arg[_]]) : LiteralStatement = {
    if (args.isEmpty)
      return new LiteralStatement(statement)
    /* based on db.postgresql.PreparedStatementHolder */
    val result = new StringBuilder(statement.length + 16*args.length)
    var offset = 0
    @scala.annotation.tailrec def next() : Boolean = {
      val i = statement.indexOf('?', offset)
      if (i == -1) {
        result ++= statement.substring(offset)
        return false
      }
      result ++= statement.substring(offset, i)
      offset = i + 1
      if (offset < statement.length && statement(offset) == '?') {
        result += '?'
        offset += 1
        next()
      } else
        true
    }
    for (arg <- args) {
      if (!next())
        throw new db.exceptions.InsufficientParametersException(0 /* whatever */, args)
      result ++= arg.escaped
    }
    if (next())
      throw new db.exceptions.InsufficientParametersException(args.length+1 /* whatever */, args)
    new LiteralStatement(result.toString)
  }
}

object Statement {
  import scala.language.implicitConversions

  implicit def apply(s : String) : SimpleStatement =
    new SimpleStatement(s)
  def apply(s : String, args : Seq[Arg[_]]) : Statement =
    if (args.isEmpty) new SimpleStatement(s)
    else new PreparedStatement(s, args)
  def empty : Statement = EmptyStatement

  def join(delim : String, s : Statement*) =
    apply(s.map(_.statement).mkString(delim), s.flatMap(_.args))
}
