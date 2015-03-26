package dbrary

import play.api.{Application,Plugin,Configuration}
import com.github.mauricio.async.db

class PostgresAsyncPlugin(app : Application) extends Plugin {
  lazy val pool = {
    val config = app.configuration.getConfig("db.default").getOrElse(Configuration.empty)
    new db.pool.ConnectionPool(SQL.Connection.factory(config),
      db.pool.PoolConfiguration(
        config.getInt("max").getOrElse(16),
        config.getMilliseconds("idle").getOrElse(600000),
        config.getInt("queue").getOrElse(16)
      ))
  }

  override def onStop() {
    pool.close
  }
}
