package store

import scala.concurrent.Future
import play.api.libs.iteratee._
import macros._
import macros.async._
import site._
import models._
import dbrary._

object CSV{

  implicit val executionContext = context.foreground

  def volume(vol: Volume): Future[String] = {
    val cc = new CSVCreate

    vol.records.flatMap{r => 
      vol.containers.flatMap{
        c => c.filter(x => !(x.top)).mapAsync(_.records).map{cr => 
          val header = cc.makeHeader(r)
          val body = cc.makeRows(cr, header)

          cc.buildCSV(header, body)

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

  def makeRows(crs: Seq[Seq[(Segment, Record)]], hs: List[(Option[RecordCategory], Metric[_])]): List[Seq[Option[Measure[_]]]] = { 
    /** make a row by adding cells to a list */
    hs.map(h => crs.map(cr => makeCell(cr.map(_._2), h._1, h._2))).transpose

  }


  def makeCell(crs: Seq[Record], ocat: Option[RecordCategory], met: Metric[_]): Option[Measure[_]] = {
    /** make a cell by locating measures with category and metric */
    crs.find(cr => cr.category == ocat)
        .flatMap(cr => cr.measures.list.find(_.metric == met))

  }

  def buildCSV(head: List[(Option[RecordCategory], Metric[_])], body: List[Seq[Option[Measure[_]]]]): String = {

    val headVals = head.map(h => h._2.name).mkString(",") + "\n"
    val rowVals = body.map(rows => rows.map(r => r match{
        case Some(measure) => measure.datum
        case None => ""
      }).mkString(",") + "\n").mkString

    headVals + rowVals

  }    


  def escapeCSV(s: String): String = {
    if(s.contains("\"") || s.contains("\n") || s.contains(",")){
      "\"" + s.replace("\"", "\"\"") + "\""
    } else {
      s
    }

  }
}





