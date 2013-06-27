package dbrary

/*
import language.experimental.macros

abstract class Enum extends Enumeration

abstract class PGenum(val pgType : String, val enumType : Enum) extends PGobject {
  setType(pgType)
  implicit def enum : enumType.Value = enumType.withName(value)
}

object PGenum {
  import scala.reflect.macros._

  def makeImpl[A : c.WeakTypeTag, B : c.WeakTypeTag](c : Context)(s : c.Expr[String])(config : c.Expr[sqltyped.Configuration[A, B]]) : c.Expr[Any] = c.abort(c.enclosingPosition, "Not yet implemented")

  def make[A, B](s : String)(implicit config : sqltyped.Configuration[A, B]) = macro makeImpl[A,B]
}
*/

object Permission extends Enumeration {
  val NONE, VIEW, DOWNLOAD, CONTRIBUTE, ADMIN = Value
  // aliases or equivalent permissions (do not use val here)
  def EDIT = CONTRIBUTE
  def DATA = DOWNLOAD
  def OWN = ADMIN
}

final class PGpermission(s : Permission.Value) extends PGtype[Permission.Value]("permission", s.toString) {
  def unPG = Permission.withName(value)
  def this() = this(Permission.NONE)
}
object PGpermission extends PGType[PGpermission,Permission.Value](new PGpermission(_))


object AuditAction extends Enumeration {
  val login, logout, add, change, remove, download = Value
}

final class PGaudit_action(s : AuditAction.Value) extends PGtype[AuditAction.Value]("audit_action", Option(s).fold("")(_.toString)) {
  def unPG = AuditAction.withName(value)
  def this() = this(null)
}
object PGaudit_action extends PGType[PGaudit_action,AuditAction.Value](new PGaudit_action(_))
