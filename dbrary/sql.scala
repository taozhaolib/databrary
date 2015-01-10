package dbrary

package object SQL {
  private[SQL] val logger : play.api.Logger = play.api.Logger("sql")
  private[SQL] def quoted(s : String) =
    "'" + s.replaceAllLiterally("'", "''") + "'";

  implicit final class Interpolator(private val sc: StringContext) {
    object sql extends ArgsView[PreparedStatement] {
      protected def result(args : Arg[_]*) =
        new PreparedStatement(sc.s(args.map(_.statement) : _*), args)
    }
    object lsql extends ArgsView[LiteralStatement] {
      protected def result(args : Arg[_]*) =
        new LiteralStatement(sc.s(args.map(_.escaped) : _*))
    }
  }
}
