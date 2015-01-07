package store

import scala.concurrent.Future
import scala.collection._
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
          val body = cc.makeRows(cr, header)
          cc.buildCSV(header, body)
        }
      }
    }
  }
}

private class CSVCreate {


  def makeHeader(cats: List[Option[RecordCategory]], recs: Seq[Record]): List[(Option[RecordCategory], Metric[_])] = {

    val m = mutable.Map.empty[Option[RecordCategory], mutable.Set[Metric[_]]]

    for (r <- recs){
      val s = m.getOrElseUpdate(r.category, mutable.Set.empty[Metric[_]])
      s ++= r.measures.list.map(_.metric)

    } 

    cats.flatMap(c => m(c).toList.sortWith(_._id < _._id).map(m => (c, m)))

  }

  def makeCSVTemplate(crs: Seq[Seq[(Segment, Record)]]): List[Option[RecordCategory]] = {
    val rs = crs.map(_.map(c => c._2))

    rs.map(_.map(f => f.category))
      .map(_.groupBy(_.map(_._id)))
      .foldLeft(Map[Option[Int], Seq[Option[RecordCategory]]]()){
        (a, b) => a ++ b.map{
          case (k, v) => k -> ( if (v.size > a.getOrElse(k, List()).size) v else a(k))
        }
      }.values.flatten.toList.sortBy(_.map(_.id._id))
  }

  def makeRows(crs: Seq[Seq[(Segment, Record)]], hs: List[(Option[RecordCategory], Metric[_])]): List[Seq[Option[Measure[_]]]] = { 
    hs.map(h => crs.map(cr => makeCell(cr.map(_._2), h._1, h._2))).transpose

  }


  def makeCell(crs: Seq[Record], ocat: Option[RecordCategory], met: Metric[_]): Option[Measure[_]] = {
    crs.find(cr => cr.category == ocat) 
        .flatMap(cr => cr.measures.list.find(_.metric == met))

  }

  def buildCSV(head: List[(Option[RecordCategory], Metric[_])], body: List[Seq[Option[Measure[_]]]]): String = {
    def escapeCSV(s: String): String = {
      if(s.contains("\"") || s.contains("\n") || s.contains(",")){
        "\"" + s.replace("\"", "\"\"") + "\""
      } else {
        s
      }
    } 

    def catName(n: Option[RecordCategory]): String = {
      n match {
        case Some(t) => t.name
        case None => ""
      }
    }
    def incrementNames(names: List[String]): List[String] = {

      names.zipWithIndex.groupBy(_._1).values.toList.flatMap{

        case n :: Nil => n :: Nil
        case ns => ns.zipWithIndex.map{
          case((str, idx), i) =>

            val Array(pref, suff) = str.split("-")

            (s"$pref${i+1}-$suff", idx)
        }
      }.sortBy(_._2).map(_._1)
    }

    val headVals = incrementNames(head.map(h => List(catName(h._1), h._2.name).mkString("-"))).mkString(",") + "\n"

    val rowVals = body.map(rows => rows.map(r => r match{
        case Some(measure) => measure.datum
        case None => ""
      }).mkString(",") + "\n").mkString

    headVals + rowVals
  }    

}





