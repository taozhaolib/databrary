package ingest

/** Simple CSV parser based on RFC 4180.*/
object CSV extends scala.util.parsing.combinator.RegexParsers {
  type T = List[List[String]]

  override val skipWhitespace = false
  override protected val whiteSpace = "[ \t]".r

  private def all : Parser[T] = repsep(record, nl)
  private def record : Parser[List[String]] = record1 | field1 | success(Nil)
  private def record1 : Parser[List[String]] = (field <~ ',') ~ rep1sep(field, ',') ^^ mkList
  private def field : Parser[String] = escaped | unescaped
  private def field1 : Parser[List[String]] = (escaped | unescaped1).^^(List(_))
  private def escaped : Parser[String] = '"' ~> """([^"]|"")*""".r <~ '"' ^^ {
    _.replaceAllLiterally("\"\"", "\"")
  }
  private def unescaped : Parser[String] = "[^,\"\r\n]*".r
  private def unescaped1 : Parser[String] = "[^,\"\r\n]+".r
  private def nl = '\r'.? ~ '\n'

  /* inefficient, but no worse than any other straight-forward solution */
  private def trim(l : T) =
    if (l.last.isEmpty) l.init else l

  private def result(r : ParseResult[T], ignoreBlankLines : Boolean = false) : Either[String,T] = r match {
    case Success(r, _) => Right(if (ignoreBlankLines) r.filter(_.nonEmpty) else trim(r))
    case e : NoSuccess => Left(e.msg)
  }

  /** Parse a CSV string.
    * @return Left(error) or Right(result)
    */
  def parseString(s : String, ignoreBlankLines : Boolean = false) : Either[String,T] =
    result(parseAll(all, s), ignoreBlankLines)
}
