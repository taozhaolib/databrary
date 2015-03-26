package dbrary.SQL

import scala.util.control.Exception.catching
import com.github.mauricio.async.db
import org.postgresql.Driver

object Connection {
  def factory(conf : play.api.Configuration) = {
    val c = conf.getString("url").fold(db.Configuration.Default)(db.postgresql.util.URLParser.parse(_))
    new db.postgresql.pool.PostgreSQLConnectionFactory(c.copy(
      username = conf.getString("user").getOrElse(c.username),
      password = conf.getString("password").orElse(c.password)
    ))
  }

private[dbrary] object Static {
  private def getConnection = {
    val conf = play.api.Configuration.load(new java.io.File(".")).getConfig("db.default").getOrElse(throw new Exception("No default database found in configuration."))
    val Seq(url, user, password) : Seq[String] = Seq("url", "user", "password").map(c =>
      conf.getString(c).getOrElse(throw conf.reportError(c, "Missing database " + c)))
    java.sql.DriverManager.registerDriver(new org.postgresql.Driver)
    java.sql.DriverManager.getConnection(url, user, password)
  }

  private lazy val connection = {
    val v = new scala.concurrent.SyncVar[java.sql.Connection]
    v.put(getConnection)
    v
  }

  private val localConnection = new scala.util.DynamicVariable[Option[java.sql.Connection]](None)

  def apply[A](f : java.sql.Connection => A) : A =
    localConnection.value.fold {
      val conn = connection.take
      try {
        localConnection.withValue(Some(conn)) {
          f(conn)
        }
      } finally {
        connection.put(conn)
      }
    }(f)

  def prepareStatement(sql : String) = apply(_.prepareStatement(sql))

  private lazy val enumLabelsStmt = prepareStatement("SELECT enumlabel FROM pg_enum JOIN pg_type ON (enumtypid = pg_type.oid) WHERE typname = ?")

  def enumLabels(name : String) : List[String] = {
    val stmt = enumLabelsStmt
    val buf = new scala.collection.mutable.ListBuffer[String]
    apply { _ =>
      stmt.setString(1, name)
      val res = stmt.executeQuery
      while (res.next)
        buf += res.getString(1)
    }
    buf.toList
  }
}

}
