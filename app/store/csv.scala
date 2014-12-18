package store

import scala.concurrent.Future
import play.api.libs.iteratee._
import macros._
import macros.async._
import site._
import models._

object CSV{

  implicit val executionContext = context.foreground


  def volume(vol: Volume): Future[String] = {
    val cc = new CSVCreate
    vol.records.flatMap{r => 
      vol.containers.flatMap{
        c => c.mapAsync(_.records).map{cont => 
          val header = cc.makeHeader(r)
          
          cc.makeRow(r, header).toString

        }
      }
    }
  }
}


private class CSVCreate {


  def makeHeader(l: Seq[Record]): List[(Option[RecordCategory], Metric[_])] = {
    /** get the column headers  by taking all the records and giving back only set of metrics and categories used in volume*/
    l.map(rec => (rec.category, rec.measures.list.map(_.metric).toList.sortWith(_._id < _._id)))
       .distinct
       .filter(x => !(x._2.isEmpty))
       .sortBy(_._1 match{ case Some(thing) => thing._id})
       .map(f => f._2.map(d => (f._1, d)))
       .flatten
       .toList
  }

  def makeRow(rs: Seq[Record], hs: List[(Option[RecordCategory], Metric[_])]): Seq[Option[Measure[_]]] = { 
    /** make a row by adding cells to a list */
    hs.map(h => makeCell(rs, h._1, h._2))
  }




  def makeCell(rs: Seq[Record], ocat: Option[RecordCategory], met: Metric[_]): Option[Measure[_]] = {
    /** make a cell by locating measures with category and metric */
    rs.find(r => r.category == ocat)
        .flatMap(r => r.measures.list.find(_.metric == met))

  }

    
}





