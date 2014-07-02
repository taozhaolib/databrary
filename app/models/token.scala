package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._

sealed abstract class Token protected (val id : Token.Id, val expires : Timestamp) extends TableRow {
  private[models] def sqlKey = SQLTerms('token -> id)
  def valid = expires.toDateTime.isAfterNow
  def auth = play.api.libs.Crypto.sign(id)
  def redeemURL = controllers.routes.TokenHtml.token(id, auth)
  def remove : Future[Boolean]
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
  protected final def tokenColumns = Columns(
      SelectColumn[Token.Id]("token")
    , SelectColumn[Timestamp]("expires")
    )
  protected def row : Selector[T]

  def delete(token : String) : Future[Boolean] =
    DELETE('token -> token).execute

  def get(token : String) : Future[Option[T]] =
    row.SELECT("WHERE token = ?").apply(token).singleOpt

  private def insert[A](f : Token.Id => Future[A]) : Future[A] = {
    /*@scala.annotation.tailrec*/ def loop : Future[A] =
      f(Token.generate).recoverWith {
        case SQLDuplicateKeyException() => loop
      }
    loop
  }
  protected def insert[A](args : SQLTerms, returning : Selector[A]) : Future[A] =
    insert[A] { (token : Token.Id) =>
      val a = ('token -> token) +: args
      SQL("INSERT INTO", table, a.insert, "RETURNING", returning.select)
        .apply(a).single(returning.parse)
    }
}

sealed abstract class AccountToken protected (id : Token.Id, expires : Timestamp, val account : Account) extends Token(id, expires) {
  def accountId = account.id
}

object AccountToken extends Table[AccountToken]("account_token") {
  private[models] def clearAccount(account : Account.Id, except : Option[Token] = None) : Future[Boolean] =
    SQL("DELETE FROM", table, "WHERE account = ? AND token <> ?")
    .apply(account, except.fold("")(_.id))
    .execute
}

/** A token, usually sent via email, granting automatic login or registration to the given party.
  * @constructor
  * @param password allow a password reset if true; just login (and require a password for changes) otherwise
  */
final class LoginToken protected (id : Token.Id, expires : Timestamp, account : Account, val password : Boolean) extends AccountToken(id, expires, account) {
  def remove = LoginToken.delete(id)

  def json = JsonRecord(id
    , 'party -> accountId
    , 'auth -> auth
    , 'reset -> account.password.nonEmpty
    )

}

object LoginToken extends TokenTable[LoginToken]("login_token") {
  private val columns = (tokenColumns ~+
      SelectColumn[Boolean]("password")
    ).map { (token, expires, password) =>
      (account : Account) => new LoginToken(token, expires, account, password)
    }
  protected val row = columns
    .join(Account.row, "login_token.account = account.id").map(tupleApply)

  /** Issue a new token for the given party.
    * @param password if this token allows a password reset. There can be only one active password reset per user at a time, so this deletes any previous ones.
    */
  def create(account : Account, password : Boolean = false) : Future[LoginToken] =
    (if (password) DELETE('account -> account.id, 'password -> true).execute
    else async(false)).flatMap { _ =>
      insert(SQLTerms('account -> account.id, 'password -> password),
        columns.map(_(account)))
    }
}

/** A token set in a cookie designating a particular session.
  */
final class SessionToken protected (id : Token.Id, expires : Timestamp, account : Account, val access : Access) extends AccountToken(id, expires, account) {
  assert(account === access.identity)
  def superuser(implicit request : play.api.mvc.Request[_]) : Boolean =
    access.isAdmin &&
      request.session.get("superuser").flatMap(Maybe.toLong _).exists(_ > System.currentTimeMillis)
  def remove = SessionToken.delete(id)
}

object SessionToken extends TokenTable[SessionToken]("session") {
  private val columns = tokenColumns
    .map { (token, expires) =>
      (account : Account, access : Access) =>
	new SessionToken(token, expires, account, access)
    }
  protected val row = columns
    .join(Account.row, "session.account = account.id")
    .leftJoin(Authorization.columns, "authorize_view.child = session.account AND authorize_view.parent = 0")
    .map { case ((t, a), p) => t(a, Authorization.make(a.party)(p)) }

  /** Issue a new token for the given party. */
  def create(account : Account) : Future[SessionToken] =
    account.party.access.flatMap { access =>
      insert(SQLTerms('account -> account.id),
        columns.map(_(account, access)))
    }
}

/** A token issued as a resumable upload identifier.
  */
final class UploadToken protected (id : Token.Id, expires : Timestamp, account : Account) extends AccountToken(id, expires, account) {
  def file = store.Upload.file(this)
  def remove = UploadToken.delete(id)
}

object UploadToken extends TokenTable[UploadToken]("upload") {
  private val columns = tokenColumns
  protected val row = columns
    .join(Account.row, "session.account = account.id")
    .map { case ((token, expires), account) =>
      new UploadToken(token, expires, account)
    }
  protected def rowAccount(account : Account) = columns
    .map { (token, expires) =>
      new UploadToken(token, expires, account)
    }

  def get(token : String, size : Long)(implicit site : AuthSite) : Future[Option[UploadToken]] =
    rowAccount(site.account)
    .SELECT("WHERE token = ? AND account = ?")
    .apply(token, site.account.id).singleOpt
    .map(_.filter(_.file.length == size))

  /** Issue a new token for a new upload. */
  def create(size : Long)(implicit site : AuthSite) : Future[UploadToken] =
    insert(SQLTerms('account -> site.account.id),
      rowAccount(site.account))
    .map { u =>
      store.Upload.writing(u, _.setLength(size))
      u
    }
}
