package ingest

/** Simple CSV parser based on RFC 4180.
  * Throws a ParseException on failure. */
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

  /* inefficient, but no worse than any straight-forward parser-based solution */
  private def trim(l : T) =
    if (l.last.isEmpty) l.init else l

  private def result(r : ParseResult[T], ignoreBlankLines : Boolean) : T = r match {
    case Success(r, _) if ignoreBlankLines => r.filter(_.nonEmpty)
    case Success(r, _) => trim(r)
    case e : NoSuccess => throw ParseException(e.msg, line = e.next.pos.line, column = e.next.pos.column)
  }

  def parseString(s : String, ignoreBlankLines : Boolean = true) : T =
    result(parseAll(all, s), ignoreBlankLines)

  def parseFile(f : java.io.File, ignoreBlankLines : Boolean = false) : T =
    result(parseAll(all, new java.io.FileReader(f)), ignoreBlankLines)
}
