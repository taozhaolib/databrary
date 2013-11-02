package models

import dbrary._
import site._

sealed class Token protected (val token : String, val expires : Timestamp) extends TableRow {
  def valid = expires.after(new java.util.Date)
  def redeemURL = controllers.routes.Token.token(token)
}

/** A token, usually sent via email, granting automatic login or registration to the given party.
  * @constructor
  * @param password allow a password reset if true; just login (and require a password for changes) otherwise
  */
final class LoginToken protected (token : String, expires : Timestamp, val account : Account, val password : Boolean) extends Token(token, expires) {
  def accountId = account.id
  def remove(implicit db : Site.DB) = LoginToken.delete(token)
}

private[models] sealed abstract class TokenTable[T <: Token](table : String) extends Table[T](table) {
  def delete(token : String)(implicit db : Site.DB) : Unit =
    SQL("DELETE FROM " + table + " WHERE token = {token}").on('token -> token).execute()

  def get(token : String)(implicit db : Site.DB) : Option[T] =
    row.SQL("WHERE token = {token}").on('token -> token).singleOpt
}

object Token extends TokenTable[Token]("token") {
  private def make(token : String, expires : Timestamp) =
    new Token(token, expires)
  private[models] val row = Columns[
    String, Timestamp](
    'token, 'expires)
    .map(make _)
}

object LoginToken extends TokenTable[LoginToken]("login_token") {
  private def make(account : Account)(token : String, expires : Timestamp, password : Boolean) =
    new LoginToken(token, expires, account, password)
  private val columns = Columns[
    String, Timestamp, Boolean](
    'token, 'expires,  'password)
  private[models] val row = columns.join(Account.row, "login_token.account = account.id").
    map { case t ~ p => (make(p) _).tupled(t) }

  /** Issue a new token for the given party.
    * @param password if this token allows a password reset. There can be only one active password reset per user at a time, so this deletes any previous ones.
    */
  def create(account : Account, password : Boolean = false)(implicit db : Site.DB) : LoginToken = {
    if (password)
      SQL("DELETE FROM login_token WHERE account = {account} AND password").on('account -> account.id).execute
    val args = SQLArgs('account -> account.id, 'password -> password)
    SQL("INSERT INTO login_token " + args.insert + " RETURNING " + columns.select)
      .on(args : _*).single(columns.map(make(account) _))
  }

  private[models] def clearAccount(account : Account.Id)(implicit db : Site.DB) : Unit =
    SQL("DELETE FROM login_token WHERE account = {account}").on('account -> account).execute
}
