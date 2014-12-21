package dbrary

import scala.concurrent.{Future,ExecutionContext}
import com.github.mauricio.async.db

trait Statement {
  def statement : String
  def args : Seq[SQLArg[_]]

  def toPrepared : PreparedStatement = PreparedStatement(statement, args)
  def toLiteral : LiteralStatement = LiteralStatement(statement, args)
  override def toString : String = toLiteral.toString
  
  def ++(s : Statement) : Statement
  def ++:(s : Statement) : Statement
  def :+(s : String) : Statement
  def +:(s : String) : Statement
  final def +(s : String) : Statement = this.:+(s)
  def :+(a : SQLArg[_]) : Statement
  def +:(a : SQLArg[_]) : Statement
  final def +(a : SQLArg[_]) : Statement = this.:+(a)
  final def ?:[A : SQLType](a : A) : Statement = this.+:(new SQLArg[A](a))
  final def :?[A : SQLType](a : A) : Statement = this.:+(new SQLArg[A](a))
}

class StatementProxy(val self : Statement) extends Statement with Proxy {
  final def statement = self.statement
  final def args = self.args

  def ++(s : Statement) = self.++(s)
  def ++:(s : Statement) = self.++:(s)
  def :+(s : String) = self.:+(s)
  def +:(s : String) = self.+:(s)
  def :+(a : SQLArg[_]) = self.:+(a)
  def +:(a : SQLArg[_]) = self.+:(a)
}

class SimpleStatement(val statement : String) extends Statement {
  final def args = Nil
  override def toString = statement

  def ++(s : Statement) = Statement(statement + s.statement, s.args)
  def ++:(s : Statement) = Statement(s.statement + statement, s.args)
  def :+(s : String) = new SimpleStatement(statement + s)
  def +:(s : String) = new SimpleStatement(s + statement)
  def :+(a : SQLArg[_]) : Statement = toPrepared.:+(a)
  def +:(a : SQLArg[_]) : Statement = toPrepared.+:(a)
}

trait SQL extends Statement {
  def ++(s : Statement) : SQL
  def ++:(s : Statement) : SQL

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
        logger.trace(((System.nanoTime - t0) / 1e9).formatted("%8.5f: ") + (if (a.nonEmpty) "[" + a.length + "]" else "") + s)
      }
    }
    r
  }

  def execute(implicit dbc : db.Connection, context : ExecutionContext) : Future[Unit] =
    send(dbc, context).map(_ => ())
  def apply(implicit dbc : db.Connection, context : ExecutionContext) : SQLResult =
    new SQLResult(send(dbc, context))
  def as[A](parse : SQLRow[A])(implicit dbc : db.Connection, context : ExecutionContext) : SQLRows[A] =
    new SQLRows(send(dbc, context), parse)
}

final class PreparedStatement(val statement : String, val args : Seq[SQLArg[_]]) extends Statement with SQL {
  override def toPrepared = this
  def ++(s : Statement) = new PreparedStatement(statement + s.statement, args ++ s.args)
  def ++:(s : Statement) = new PreparedStatement(s.statement + statement, s.args ++: args)
  def :+(s : String) = new PreparedStatement(statement + s, args)
  def +:(s : String) = new PreparedStatement(s + statement, args)
  def :+(a : SQLArg[_]) = new PreparedStatement(statement + a.placeholder, args :+ a)
  def +:(a : SQLArg[_]) = new PreparedStatement(a.placeholder + statement, a +: args)
}

object PreparedStatement {
  def apply(statement : String, args : Seq[SQLArg[_]]) : PreparedStatement =
    new PreparedStatement(statement, args)
}

final class LiteralStatement(statement : String) extends SimpleStatement(statement) with SQL {
  override def toLiteral = this
  override def ++(s : Statement) = new LiteralStatement(statement + s.toLiteral.statement)
  override def ++:(s : Statement) = new LiteralStatement(s.toLiteral.statement + statement)
  override def :+(s : String) = new LiteralStatement(statement + s)
  override def +:(s : String) = new LiteralStatement(s + statement)
  override def :+(a : SQLArg[_]) = this.:+(a.escaped)
  override def +:(a : SQLArg[_]) = this.+:(a.escaped)
}

object LiteralStatement {
  def apply(statement : String, args : Seq[SQLArg[_]]) : LiteralStatement = {
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

  def apply(s : String) : SimpleStatement =
    new SimpleStatement(s)
  def apply(s : String, args : Seq[SQLArg[_]]) : Statement =
    if (args.isEmpty) new SimpleStatement(s)
    else new PreparedStatement(s, args)

  def join(delim : String, s : Statement*) =
    apply(s.map(_.statement).mkString(delim), s.flatMap(_.args))
}
