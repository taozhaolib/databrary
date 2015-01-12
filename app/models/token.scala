package models

import scala.concurrent.{ExecutionContext,Future}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.Crypto
import macros._
import dbrary._
import dbrary.SQL._
import site._

trait TokenAuth {
  def token : String
  def auth() : String = {
    val t = System.currentTimeMillis.toHexString
    Crypto.sign(token + t) + t
  }
  def checkAuth(a : String) : Boolean = {
    val (s, t) = a.splitAt(TokenAuth.length)
    Crypto.constantTimeEquals(s, Crypto.sign(token + t))
  }
}

object TokenAuth {
  private final val length = Crypto.sign("").length
}

sealed abstract class Token protected (val id : Token.Id, val expires : Timestamp) extends TableRow with TokenAuth {
  private[models] def sqlKey = SQLTerms('token -> id)
  def token = id
  def valid = expires.toDateTime.isAfterNow
  def redeemURL = controllers.routes.TokenHtml.token(id, auth, None)
  def remove : Future[Boolean]
}

object Token extends Table[Token]("token") {
  type Id = String
  final val length = 32

  def clean(implicit defaultContext : ExecutionContext) : Future[Boolean] =
    for {
      r <- UploadToken.clean
      _ <- (lsql"DELETE FROM " + table + " WHERE expires < CURRENT_TIMESTAMP").execute
    } yield (r)

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

  def remove(token : String) : Future[Boolean] =
    DELETE('token -> token.take(Token.length)).execute

  def get(token : String) : Future[Option[T]] =
    row.SELECT(sql"WHERE token = $token").singleOpt

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
      (lsql"INSERT INTO " + table + " " ++ a.insert + " RETURNING " ++ returning.select)
      .run.single(returning.parse)
    }
}

sealed abstract class AccountToken protected (id : Token.Id, expires : Timestamp, val account : Account) extends Token(id, expires) {
  def accountId = account.id
}

object AccountToken extends Table[AccountToken]("account_token") {
  private[models] def clearAccount(account : Account.Id, except : Option[Token] = None) : Future[Boolean] =
    (lsql"DELETE FROM " + table ++ lsql" WHERE account = $account" ++ except.fold(Statement.empty)(e => lsql" AND token <> ${e.id}"))
    .run.execute
}

/** A token, usually sent via email, granting automatic login or registration to the given party.
  * @constructor
  * @param password allow a password reset if true; just login (and require a password for changes) otherwise
  */
final class LoginToken protected (id : Token.Id, expires : Timestamp, account : Account, val password : Boolean) extends AccountToken(id, expires, account) {
  def remove = LoginToken.remove(id)

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
    .join(Account.row on "login_token.account = account.id").map(tupleApply)

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
  def remove = SessionToken.remove(id)
}

object SessionToken extends TokenTable[SessionToken]("session") {
  private val columns = tokenColumns
    .map { (token, expires) =>
      (account : Account, access : Access) =>
        new SessionToken(token, expires, account, access)
    }
  protected val row = columns.join(
      Authorization.rowParent() on "session.account = account.id"
    ).map { case (t, auth) => t(auth.child.account.get, auth) }

  /** Issue a new token for the given party. */
  def create(account : Account) : Future[SessionToken] =
    account.party.access.flatMap { access =>
      insert(SQLTerms('account -> account.id),
        columns.map(_(account, access)))
    }
}

/** A token issued as a resumable upload identifier.
  */
final class UploadToken protected (id : Token.Id, val filename : String, expires : Timestamp, account : Account) extends AccountToken(id, expires, account) {
  def file = store.Upload.file(id)
  def remove = UploadToken.remove(id)
}

object UploadToken extends TokenTable[UploadToken]("upload") {
  private val columns = (tokenColumns ~+
      SelectColumn[String]("filename")
    ).map { (token, expires, filename) =>
      (account : Account) => new UploadToken(token, filename, expires, account)
    }
  protected val row = columns
    .join(Account.row on "session.account = account.id")
    .map(tupleApply)
  protected def rowAccount(account : Account) = columns
    .map(_(account))

  private[models] def clean(implicit defaultContext : ExecutionContext) : Future[Boolean] =
    lsql"DELETE FROM upload WHERE expires < CURRENT_TIMESTAMP RETURNING token"
    .run.list(SQL.Cols[String])
    .map(_.map(store.Upload.file(_).delete).forall(identity))

  def get(token : String)(implicit site : AuthSite) : Future[Option[UploadToken]] =
    rowAccount(site.account)
    .SELECT(sql"WHERE token = $token AND account = ${site.identity.id}")
    .singleOpt

  def take(token : String)(implicit site : AuthSite) : Future[Option[UploadToken]] =
    rowAccount(site.account)
    .run((select, source) =>
      lsql"DELETE FROM upload WHERE token = $token AND account = ${site.identity.id} RETURNING " ++ select)
    .singleOpt

  /** Issue a new token for a new upload. */
  def create(filename : String)(implicit site : AuthSite) : Future[UploadToken] =
    insert(SQLTerms('account -> site.account.id, 'filename -> filename),
      rowAccount(site.account))
}
