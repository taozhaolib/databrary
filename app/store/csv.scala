package store

import scala.concurrent.Future
import play.api.libs.iteratee._
import macros._
import macros.async._
import site._
import models._

private class CSVParse {
  def parseListofList[T](input: Seq[T]) = input match{
    case x :: xs => input.head.toString 
    
  }

  def csvColumnize(l: Seq[Record]): Seq[(Int, Iterable[Int], String)] = {
    /** get the column headers  */
    l.map(rec => (rec._id, rec.measures.list.map(_.metric._id), rec.category match{
        case Some(thing) => thing.name
        case _ => "what"
      }))
  }
}



object CSV{

  implicit val executionContext = context.foreground


  def volume(vol: Volume): Future[String] = {
    val cparse = new CSVParse
    vol.records.flatMap{r => 
      vol.containers.flatMap{
        c => c.mapAsync(_.records).map{cont => 
          /**cparse.parseListofList(c.zip(cont))*/
          cparse.csvColumnize(r).toString
        }
      }
    }

  }


}

