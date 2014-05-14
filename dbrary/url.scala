package dbrary

import java.net._

object URLStreamHandlerFactoryImpl extends URLStreamHandlerFactory {
  sealed abstract class TransformedURLHandler extends URLStreamHandler {
    protected def transform(u : URL) : URL
    final protected def openConnection(u : URL) : URLConnection =
      transform(u).openConnection()
    final override protected def openConnection(u : URL, p : Proxy) : URLConnection =
      transform(u).openConnection(p)
  }

  object DOIHandler extends TransformedURLHandler {
    private val DOIRegex = "10\\.[\\.0-9]+/".r

    protected def transform(u : URL) =
      new URL("http", "dx.doi.org", "/" + u.getFile)
    override protected def parseURL(u : URL, spec : String, start : Int, limit : Int) {
      val doi = spec.substring(start, limit)
      if (DOIRegex.findPrefixOf(doi).isEmpty)
	throw new RuntimeException("Invalid DOI")
      setURL(u, "doi", u.getHost, u.getPort, u.getAuthority, u.getUserInfo, doi, u.getQuery, u.getRef)
    }
  }

  object HDLHandler extends TransformedURLHandler {
    protected def transform(u : URL) =
      new URL("http", "hdl.handle.net", "/" + u.getFile)
    override protected def parseURL(u : URL, spec : String, start : Int, limit : Int) {
      val hdl = spec.substring(start, limit)
      setURL(u, "hdl", u.getHost, u.getPort, u.getAuthority, u.getUserInfo, hdl, u.getQuery, u.getRef)
    }
  }

  def createURLStreamHandler(protocol : String) : URLStreamHandler = protocol match {
    case "doi" => DOIHandler
    case "hdl" => HDLHandler
    case _ => null
  }

  def normalize(u : URL) : URL =
    u.openConnection.getURL
}
