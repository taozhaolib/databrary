package dbrary 

private[dbrary] object dbutil {
  // ugly ugly but safe enough. what else to do?
  val timestampUtils = {
    val c = classOf[org.postgresql.jdbc2.TimestampUtils].getDeclaredConstructor(classOf[Boolean], classOf[Boolean], classOf[Boolean])
    c.setAccessible(true)
    c.newInstance(java.lang.Boolean.TRUE, java.lang.Boolean.TRUE, java.lang.Boolean.FALSE)
  }
}
