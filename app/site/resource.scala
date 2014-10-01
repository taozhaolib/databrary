package site

import java.io.File
import scala.collection.JavaConversions.enumerationAsScalaIterator
import play.api.Application

object Resource {
  def list(dir : String)(implicit app : play.api.Application) : Iterable[String] =
    app.resource(dir).fold[Iterable[String]](Nil) { url =>
      url.getProtocol match {
        case "file" => new File(url.toURI).list.filter(_.head != '.')
        case "jar" =>
          val jc = url.openConnection.asInstanceOf[java.net.JarURLConnection]
          val je = jc.getEntryName + "/"
          jc.getJarFile.entries.map(_.getName).filter(e => e.startsWith(je) && e.length > je.length).map(_.drop(je.length)).toIterable
        case _ => Nil
      }
    }
}
