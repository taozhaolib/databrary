package store

import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.iteratee._
import macros._
import macros.async._
import site._
import models._
import dbrary._

object CSV {
  implicit private val executionContext = context.foreground

  def volume(vol: Volume): Future[String] = {
    vol.records.flatMap{r =>
      vol.containers.flatMap{ ca =>
        val c = ca.filter(!_.top)
        c.mapAsync(_.records).map{crs =>
          val rml: Seq[Map[Option[RecordCategory], Seq[Record]]] = crs.map(_.map(_._2).groupBy(_.category))
          val header: Seq[(Option[RecordCategory], Int, Seq[Metric[_]])] = makeHeader(maxCats(rml), headerData(r))
          val body: Seq[Seq[String]] = rml.map(row(header, _))
          buildCSV(header, body, cData(c))
        }
      }
    }
  }

  private def maxCats[A](ml: Seq[Map[A, Seq[_]]]): collection.Map[A, Int] = {
    val r = mutable.Map.empty[A, Int]
    for (m <- ml ; (k, v) <- m) {
      r.update(k, v.size.max(r.getOrElse(k, 0)))
    }
    r
  }

  private def headerData(rs: Seq[Record]): collection.Map[Option[RecordCategory], collection.Set[Metric[_]]] = {
    val m = mutable.Map.empty[Option[RecordCategory], mutable.Set[Metric[_]]]
    for (r <- rs) {
      val s = m.getOrElseUpdate(r.category, mutable.Set.empty[Metric[_]])
      s ++= r.measures.list.map(_.metric)
    }
    m
  }

  private def makeHeader(rc: collection.Map[Option[RecordCategory], Int], rm: collection.Map[Option[RecordCategory], collection.Set[Metric[_]]]): Seq[(Option[RecordCategory], Int, Seq[Metric[_]])] = {
    rc.keys.toSeq.sortBy(_.map(_.id._id)).map { r =>
      (r, rc(r), rm(r).toSeq.sortBy(_._id))
    }
  }

  private def row(h: Seq[(Option[RecordCategory], Int, Seq[Metric[_]])], data: Map[Option[RecordCategory], Seq[Record]]): Seq[String] = {
    h.flatMap { case (c, i, ml) =>
      val recList:Seq[Record] = data.getOrElse(c, Nil)
      recList.flatMap { r =>
        ml.map { m =>
          r.measures(m).fold("")(_.datum)
        }
      } ++ Seq.fill((i - recList.length) * ml.length)("")
    }
  }

  private def cData(cs: Seq[Container]): Seq[Seq[String]] = {
    cs.map(c => Seq(c._id.toString, c.name.getOrElse(""), c.getDate.fold("")(_.toString)))
  }

  private def escapeCSV(s: String): String = {
    if (s.contains("\"") || s.contains("\n") || s.contains(","))
      "\"" + s.replaceAllLiterally("\"", "\"\"") + "\""
    else
      s
  }

  private def expandHeader(h: Seq[(Option[RecordCategory], Int, Seq[Metric[_]])]): Seq[String] = {
    h.flatMap { case (c, n, ml) =>
      val cn = c.fold("")(_.name)
      Seq.range(0, n).flatMap{ i =>
        val ci = cn + (if (i > 0) (i+1).toString else "") + "-"
        ml.map(ci + _.name)
      }
    }
  }

  private def buildCSV(header: Seq[(Option[RecordCategory], Int, Seq[Metric[_]])], body: Seq[Seq[String]], cd: Seq[Seq[String]]): String = {
    val containerHeads = Seq("session-id", "session-name", "session-date")
    ((containerHeads, expandHeader(header)) +: cd.zip(body))
      .map(v => (v._1 ++ v._2).map(escapeCSV).mkString(",")).mkString("", "\n", "\n")
  }

}
