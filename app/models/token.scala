package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

sealed abstract class Token protected (val token : String, val expires : Timestamp) extends TableRow {
  def valid = expires.isAfterNow
  def redeemURL = controllers.routes.Token.token(token)
  def remove : Future[Boolean]
}

sealed abstract class AccountToken protected (token : String, expires : Timestamp, val account : Account) extends Token(token, expires) {
  def accountId = account.id
}

/** A token, usually sent via email, granting automatic login or registration to the given party.
  * @constructor
  * @param password allow a password reset if true; just login (and require a password for changes) otherwise
  */
final class LoginToken protected (token : String, expires : Timestamp, account : Account, val password : Boolean) extends AccountToken(token, expires, account) {
  def remove = LoginToken.delete(token)
}

/** A token set in a cookie designating a particular session.
  */
final class SessionToken protected (token : String, expires : Timestamp, account : Account, val access : Permission.Value) extends AccountToken(token, expires, account) {
  def superuser(implicit request : play.api.mvc.Request[_]) : Boolean =
    access == Permission.ADMIN &&
      request.session.get("superuser").flatMap(Maybe.toLong _).exists(_ > System.currentTimeMillis)
  def remove = SessionToken.delete(token)
}

private[models] sealed abstract class TokenTable[T <: Token](table : String) extends Table[T](table) {
  protected def row : Selector[T]

  def delete(token : String) : Future[Boolean] =
    DELETE('token -> token).execute

  def get(token : String) : Future[Option[T]] =
    row.SELECT("WHERE token = ?").apply(token).singleOpt
}

object Token extends Table[Token]("token") {
  private[models] def clean() : Future[Boolean] =
    SQL("DELETE FROM", table, "WHERE expires < CURRENT_TIMESTAMP").apply().execute
}

object AccountToken extends Table[AccountToken]("account_token") {
  private[models] def clearAccount(account : Account.Id, except : Option[Token] = None) : Future[Boolean] =
    except.fold {
      DELETE('account -> account)
    } { token =>
      SQL("DELETE FROM", table, "WHERE account = ? AND token <> ?").apply(account, token.token)
    }.execute
}

object LoginToken extends TokenTable[LoginToken]("login_token") {
  private val columns = Columns(
      SelectColumn[String]("token")
    , SelectColumn[Timestamp]("expires")
    , SelectColumn[Boolean]("password")
    ).map { (token, expires, password) =>
      (account : Account) => new LoginToken(token, expires, account, password)
    }
  protected val row = columns.join(Account.row, "login_token.account = account.id").
    map { case (t, p) => t(p) }

  /** Issue a new token for the given party.
    * @param password if this token allows a password reset. There can be only one active password reset per user at a time, so this deletes any previous ones.
    */
  def create(account : Account, password : Boolean = false) : Future[LoginToken] =
    (if (password) DELETE('account -> account.id, 'password -> true).execute
    else Async(false)).flatMap { _ =>
      val args = SQLTerms('account -> account.id, 'password -> password)
      SQL("INSERT INTO", table, args.insert, "RETURNING", columns.select)
        .apply(args).single(columns.parse.map(_(account)))
    }
}

object SessionToken extends TokenTable[SessionToken]("session") {
  private val columns = Columns(
      SelectColumn[String]("token")
    , SelectColumn[Timestamp]("expires")
    ).map { (token, expires) =>
      (account : Account, access : Permission.Value) => new SessionToken(token, expires, account, access)
    }
  protected val row = columns.join(Account.rowAccess, "session.account = account.id").
    map { case (t, (p, a)) => t(p, a) }

  /** Issue a new token for the given party. */
  def create(account : Account) : Future[String] = {
    val args = SQLTerms('account -> account.id)
    SQL("INSERT INTO", table, args.insert, "RETURNING token")
      .apply(args).single(SQLCols[String])
  }
}
