package ingest {
  class IngestException(message : String) extends java.lang.RuntimeException(message)
}

package object ingest {
  def optString[A](v : Option[A]) : String = v.fold("")(_.toString)
}
