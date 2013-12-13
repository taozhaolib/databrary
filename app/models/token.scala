package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

sealed abstract class Token protected (val id : Token.Id, val expires : Timestamp) extends TableRow {
  def valid = expires.isAfterNow
  def redeemURL = controllers.routes.Token.token(id)
  def remove : Future[Boolean]
}

sealed abstract class AccountToken protected (id : Token.Id, expires : Timestamp, val account : Account) extends Token(id, expires) {
  def accountId = account.id
}

/** A token, usually sent via email, granting automatic login or registration to the given party.
  * @constructor
  * @param password allow a password reset if true; just login (and require a password for changes) otherwise
  */
final class LoginToken protected (id : Token.Id, expires : Timestamp, account : Account, val password : Boolean) extends AccountToken(id, expires, account) {
  def remove = LoginToken.delete(id)
}

/** A token set in a cookie designating a particular session.
  */
final class SessionToken protected (id : Token.Id, expires : Timestamp, account : Account, val access : Permission.Value) extends AccountToken(id, expires, account) {
  def superuser(implicit request : play.api.mvc.Request[_]) : Boolean =
    access == Permission.ADMIN &&
      request.session.get("superuser").flatMap(Maybe.toLong _).exists(_ > System.currentTimeMillis)
  def remove = SessionToken.delete(id)
}

object Token extends Table[Token]("token") {
  type Id = String
  private final val length = 64

  private[models] def clean() : Future[Boolean] =
    SQL("DELETE FROM", table, "WHERE expires < CURRENT_TIMESTAMP").apply().execute

  private final val rawLength = store.Base64.decodedLength(length)
  private final val random = new java.security.SecureRandom
  private[models] def generate : Id = {
    val b = new Array[Byte](rawLength)
    random.nextBytes(b)
    new String(store.Base64(b))
  }
}

private[models] sealed abstract class TokenTable[T <: Token](table : String) extends Table[T](table) {
  protected def row : Selector[T]

  def delete(token : String) : Future[Boolean] =
    DELETE('token -> token).execute

  def get(token : String) : Future[Option[T]] =
    row.SELECT("WHERE token = ?").apply(token).singleOpt

  protected def insert[A](f : Token.Id => Future[A]) : Future[(Token.Id,A)] = {
    /*@scala.annotation.tailrec*/ def loop : Future[(Token.Id,A)] = {
      val t = Token.generate
      f(t).map((t, _)).recoverWith {
        case SQLDuplicateKeyException() => loop
      }
    }
    loop
  }
  protected def insert(args : SQLTerms) : Future[Token.Id] =
    insert[Boolean] { (token : Token.Id) =>
      INSERT(('token -> token) +: args).execute
    }.map(_._1)
  protected def insert[A](args : SQLTerms, returning : Selector[A]) : Future[(Token.Id,A)] =
    insert[A] { (token : Token.Id) =>
      val a = ('token -> token) +: args
      SQL("INSERT INTO", table, args.insert, "RETURNING", returning.select)
        .apply(args).single(returning.parse)
    }
}

object AccountToken extends Table[AccountToken]("account_token") {
  private[models] def clearAccount(account : Account.Id, except : Option[Token] = None) : Future[Boolean] =
    except.fold {
      DELETE('account -> account)
    } { token =>
      SQL("DELETE FROM", table, "WHERE account = ? AND token <> ?").apply(account, token.id)
    }.execute
}

object LoginToken extends TokenTable[LoginToken]("login_token") {
  private val columns = Columns(
      SelectColumn[Token.Id]("token")
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
      insert(SQLTerms('account -> account.id, 'password -> password),
        columns.map(_(account)))
        .map(_._2)
    }
}

object SessionToken extends TokenTable[SessionToken]("session") {
  private val columns = Columns(
      SelectColumn[Token.Id]("token")
    , SelectColumn[Timestamp]("expires")
    ).map { (token, expires) =>
      (account : Account, access : Permission.Value) => new SessionToken(token, expires, account, access)
    }
  protected val row = columns.join(Account.rowAccess, "session.account = account.id").
    map { case (t, (p, a)) => t(p, a) }

  /** Issue a new token for the given party. */
  def create(account : Account) : Future[SessionToken] =
    account.party.access.flatMap { access =>
      insert(SQLTerms('account -> account.id),
        columns.map(_(account, access)))
        .map(_._2)
    }
}
