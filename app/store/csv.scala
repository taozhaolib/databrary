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
         
          val rl: Seq[Seq[Record]] = crs.map(z => z.map(_._2))         
          val cl: Seq[Container] = c
          val rml: Seq[Map[Option[RecordCategory], Seq[Record]]] = rl.map(cc.groupCats)
          val rcsize: Seq[Map[Option[RecordCategory], Int]] = rml.map(_.mapValues(_.size))
          val rcmax: Map[Option[RecordCategory], Int] = rcsize.fold(Map.empty)(cc.maxCats)
          val rmets: Map[Option[RecordCategory], Set[Metric[_]]] = cc.headerData(r)
          val header: Seq[(Option[RecordCategory], Int, List[Metric[_]])] = cc.makeHeader(rcmax, rmets)
          val body: Seq[Seq[String]] = rml.map(r => cc.row(header, r))
          val containerData: Seq[List[String]] = cc.cData(c.filter(x => !(x.top))) 
          cc.buildCSV(header, body, containerData)
          
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
        recList.flatMap{
          case r => ml.map{ m => 

            r.measures(m).fold("")(_.datum)

          } 
        } ++ List.fill((i - recList.length) * ml.length)("")
      }

  }

  def cData(cs: Seq[Container]): Seq[List[String]] = {

    def getName(s: Option[String]): String = {
      s match{ 
        case Some(r) => r
        case None => ""
      }
    }

    def getDate(d: Option[Date]): String = 
      d match{ 
        case Some(r) => r.toString
        case None => ""
      }

    cs.map(c => List(c._id.toString, getName(c.name), getDate(c.date)))

  }
  

  def buildCSV(header: Seq[(Option[RecordCategory], Int, List[Metric[_]])], body: Seq[Seq[String]], cd: Seq[List[String]]): String = {
  
    def escapeCSV(s: String): String = {
      if(s.contains("\"") || s.contains("\n") || s.contains(",")){
        "\"" + s.replace("\"", "\"\"") + "\""
      } else {
        s
      }
    }   
    
    val cleanCSV: Seq[Seq[String]] = body.map(_.map(s => escapeCSV(s)))
    
    def expandHeader(h: Seq[(Option[RecordCategory], Int, List[Metric[_]])]): Seq[String] = {

      val h2: Seq[(Option[RecordCategory], Int, List[Metric[_]])] = h.flatMap{ case (c, i, ml) =>

        List.range(1, i+1).map{ n =>

          (c, n, ml)

        }

      }

      def catName(n: Option[RecordCategory]): String = {
       n match {
        case Some(t) => t.name
        case None => ""
       }
      }

      def incName(c: String, i: Int, m: String): List[String] = {

        val z = List.range(1, i + 1)
        z.map { n =>

          (c + n.toString + "-" + m)

        }
      }
      
      h2.flatMap{
        case (c, i, ml) => ml.map{ m =>  
          
          if ( i > 1){
            (catName(c)+ i.toString + "-" + m.name)
          } else {
            (catName(c) + "-" + m.name)

          }
        }
      }

    }


    val containerHeads = List("session-id", "session-name", "session-date")
    val headVals: String = (containerHeads ++ expandHeader(header)).mkString(",") + "\n"
    val rowVals: String = cd.zip(cleanCSV).flatMap(v => (v._1 ++ v._2).mkString(",")+"\n").mkString

    headVals + rowVals
  }    

}





