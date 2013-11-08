package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

sealed class Token protected (val token : String, val expires : Timestamp) extends TableRow {
  def valid = expires.isAfterNow
  def redeemURL = controllers.routes.Token.token(token)
}

/** A token, usually sent via email, granting automatic login or registration to the given party.
  * @constructor
  * @param password allow a password reset if true; just login (and require a password for changes) otherwise
  */
final class LoginToken protected (token : String, expires : Timestamp, val account : Account, val password : Boolean) extends Token(token, expires) {
  def accountId = account.id
  def remove = LoginToken.delete(token)
}

private[models] sealed abstract class TokenTable[T <: Token](table : String) extends Table[T](table) {
  protected def row : Selector[T]

  def delete(token : String) : Future[Boolean] =
    DELETE('token -> token).execute

  def get(token : String) : Future[Option[T]] =
    row.SELECT("WHERE token = ?").apply(token).singleOpt
}

object Token extends TokenTable[Token]("token") {
  private def make(token : String, expires : Timestamp) =
    new Token(token, expires)
  protected val row = Columns(
      SelectColumn[String]("token")
    , SelectColumn[Timestamp]("expires")
    ).map(make _)
}

object LoginToken extends TokenTable[LoginToken]("login_token") {
  private def make(account : Account)(token : String, expires : Timestamp, password : Boolean) =
    new LoginToken(token, expires, account, password)
  private val columns = Columns(
      SelectColumn[String]("token")
    , SelectColumn[Timestamp]("expires")
    , SelectColumn[Boolean]("password")
    )
  protected val row = columns.join(Account.row(Site.Anon), "login_token.account = account.id").
    map { case (t, p) => (make(p) _).tupled(t) }

  /** Issue a new token for the given party.
    * @param password if this token allows a password reset. There can be only one active password reset per user at a time, so this deletes any previous ones.
    */
  def create(account : Account, password : Boolean = false) : Future[LoginToken] =
    (if (password) DELETE('account -> account.id, 'password -> true).execute
    else Async(false)).flatMap { _ =>
      val args = SQLTerms('account -> account.id, 'password -> password)
      SQL("INSERT INTO login_token " + args.insert + " RETURNING " + columns.select)
        .apply(args).single(columns.parse.map(make(account) _))
    }

  private[models] def clearAccount(account : Account.Id) : Future[Boolean] =
    DELETE('account -> account).execute
}
