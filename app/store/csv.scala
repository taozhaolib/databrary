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
        c => c.filter(x => !(x.top)).mapAsync(_.records).map{cr=> 
          val template = cc.makeCSVTemplate(cr)
          val header = cc.makeHeader(template, r)
          /**val body = cc.makeRows(cr, header)*/
          /**cc.buildCSV(header, body)*/
          header.toString

        }
      }
    }
  }
}

private class CSVCreate {


  def makeHeader(temp: List[Int], rs: Seq[Record]): List[(Option[RecordCategory], Metric[_])] = {

    def getMatch(rc: Option[RecordCategory]): Int = {
      rc match {
        case Some(e) => e._id
        case None => 0
      }

    }

    def catId(rec: Record): Option[Int] = rec.category.map(_._id)
    
    temp.flatMap(t => rs.collect{ case r if Some(t) == catId(r) => 
        (r.category, r.measures.list.map(_.metric).toList.sortWith(_._id < _._id))
      })
       .map(f => f._2.map(d => (f._1, d)))
       .flatten
       .toList

    /**
    rs.map(r => for { 
      t <- temp
      if Some(t) == catId(r)
      } yield (r.category, r.measures.list.map(_.metric).toList.sortWith(_._id < _._id)))
       .flatMap(_.map(f => f._2.map(d => (f._1, d))))
       .flatten
       .toList
    */


  }

  def makeCSVTemplate(crs: Seq[Seq[(Segment, Record)]]): List[Int] = {
    val rs = crs.map(_.map(c => c._2))

    rs.map(_.map(f => (f.category match{ case Some(cat) => cat._id})).sorted)
      .map(_.groupBy(identity))
      .foldLeft(Map[Int, Seq[Int]]()){
        (a, b) => a ++ b.map{
          case (k, v) => k -> ( if (v.size > a.getOrElse(k, List()).size) v else a(k))
        }
      }.values.flatten.toList.sorted
  }

  def makeRows(crs: Seq[Seq[(Segment, Record)]], hs: List[(Option[RecordCategory], Metric[_])]): List[Seq[Option[Measure[_]]]] = { 
    hs.map(h => crs.map(cr => makeCell(cr.map(_._2), h._1, h._2))).transpose

  }


  def makeCell(crs: Seq[Record], ocat: Option[RecordCategory], met: Metric[_]): Option[Measure[_]] = {
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





