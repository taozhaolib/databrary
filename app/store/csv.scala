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

  def csvColumnize(l: Seq[Record]): List[(Int, Int)] = {
    /** get the column headers  by taking all the records and giving back only set of metrics and categories used in volume*/
    l.map(rec => (rec.measures.list.map(_.metric._id).toList.sortWith(_ < _), rec.category match{
        case Some(thing) => thing._id
      }))
      .distinct
      .filter(x => !(x._1.isEmpty))
      .sortBy(t => t._2)
      .map(f => f._1.map(d => (d, f._2)))
      .flatten
      .toList
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

