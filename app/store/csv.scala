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
        c => c.filter(x => !(x.top)).mapAsync(_.records).map{crs => 
          
          /**
          val template = cc.makeCSVTemplate(crs)
          val header = cc.makeHeader(template, r)
          val body = cc.makeRows(crs, header)
          val containerData = cc.cData(c.filter(x => !(x.top)))          
          cc.buildCSV(header, body, containerData)
          */
         
          val rl: Seq[Seq[Record]] = crs.map(z => z.map(_._2))         
          val cl: Seq[Container] = c
          val rml: Seq[Map[Option[RecordCategory], Seq[Record]]] = rl.map(cc.groupCats)
          val rcsize: Seq[Map[Option[RecordCategory], Int]] = rml.map(_.mapValues(_.size))
          val rcmax: Map[Option[RecordCategory], Int] = rcsize.fold(Map.empty)(cc.maxCats)
          val rmets: Map[Option[RecordCategory], Set[Metric[_]]] = cc.headerData(r)
          val header: Seq[(Option[RecordCategory], Int, List[Metric[_]])] = cc.makeHeader(rcmax, rmets)
          val body: Seq[Seq[String]] = rml.map(r => cc.row(header, r))
          body.toString

          
        }
      }
    }
  }
}

private class CSVCreate {

  def groupCats(rs: Seq[Record]): Map[Option[RecordCategory], Seq[Record]] = {
    rs.groupBy(r => r.category)
  }


  def maxCats[A](m1: Map[A, Int], m2: Map[A, Int]): Map[A, Int] = {

    m1.foldLeft(m2){case (m, (k, v)) =>

      val x = m.getOrElse(k, 0)

      m.updated(k, x.max(v))
    }

  }

  def headerData(rs: Seq[Record]): Map[Option[RecordCategory], Set[Metric[_]]] = {

    val m = mutable.Map.empty[Option[RecordCategory], mutable.Set[Metric[_]]]

    for (r <- rs) { 
      val s = m.getOrElseUpdate(r.category, mutable.Set.empty[Metric[_]])
      s ++= r.measures.list.map(_.metric)
    } 

    m

  }

  def makeHeader(rc: Map[Option[RecordCategory], Int], rm: Map[Option[RecordCategory], Set[Metric[_]]]): Seq[(Option[RecordCategory], Int, List[Metric[_]])] = {

    val rcsorted = rc.keys.toSeq.sortBy(_.map(_.id._id))

    rcsorted.map {  r =>

      (r, rc(r), rm(r).toSeq.sortBy(_._id).toList)

    }

  }

  def row(h: Seq[(Option[RecordCategory], Int, List[Metric[_]])], data: Map[Option[RecordCategory], Seq[Record]]): Seq[String] = {

      h.flatMap { case (c, i, ml) => 
        val recList:Seq[Record] = data.getOrElse(c, Nil)
        recList.flatMap{ r => 
          ml.map{ m => 

            r.measures(m).fold("")(_.datum)

          } ++ List.fill((i - recList.length) * ml.length)("")

        } 


      }

  }
  /**
  def makeHeader(cats: List[Option[RecordCategory]], recs: Seq[Record]): List[(Option[RecordCategory], Metric[_])] = {

    val m = mutable.Map.empty[Option[RecordCategory], mutable.Set[Metric[_]]]

    for (r <- recs){
      val s = m.getOrElseUpdate(r.category, mutable.Set.empty[Metric[_]])
      s ++= r.measures.list.map(_.metric)

    } 

    cats.flatMap(c => m(c).toList.sortWith(_._id < _._id).map(m => (c, m)))

  }
  */
  /**
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


  def makeCell(rs: Seq[Record], ocat: Option[RecordCategory], met: Metric[_]): Option[Measure[_]] = {
    
    rs.find(r => r.category == ocat) 
      .flatMap(r => r.measures.list.find(_.metric == met))  
    

    
    val groups = rs.groupBy(_.category).values.toList
    
    groups.filter(g => g.map(_.category) == ocat).flatMap {
      case r :: Nil => r.measures.list.find(_.metric == met)
      case rs => for (r <- rs) r.measures.list.find(_.metric == met)
    }
    
    
   
  }

  def cData(cs: Seq[Container]): Seq[List[String]] = {

    def getName(s: Option[String]): String = {
      s match{ 
        case Some(n) => n
        case None => ""
      }

    }

    def getDate(d: Option[Date]): String = 
      d match{ 
        case Some(f) => f.toString
        case None => ""
      }

    cs.map(c => List(c._id.toString, getName(c.name), getDate(c.date)))

  }
  */

  def buildCSV(head: List[(Option[RecordCategory], Metric[_])], body: List[Seq[Option[Measure[_]]]], cd: Seq[List[String]]): String = {
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

    val containerHeads = List("session id", "session name", "session date")

    val headVals = (containerHeads ++ incrementNames(head.map(h => List(catName(h._1), h._2.name).mkString("-")))).mkString(",") + "\n"

    

    val rs = body.map(rows => rows.map(r => r match{
        case Some(measure) => measure.datum
        case None => ""
      }))

    val rowVals = cd.zip(rs).flatMap(v => (v._1 ++ v._2).mkString(",")+"\n").mkString

    headVals + rowVals
  }    

}





