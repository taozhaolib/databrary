package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current
import macros._
import macros.async._
import dbrary._
import dbrary.SQL._
import site._

final class Funder(val id : Funder.Id, val name : String) extends TableRow {
  private[models] def sqlKey = SQLTerms('fundref_id -> id)
  private[models] def sqlArgs = sqlKey ++ SQLTerms('name -> name)

  def json = JsonObject(
    'id -> id,
    'name -> name
  )
}

object Funder extends Table[Funder]("funder") {
  type Id = LongId[Funder]
  def asId(i : Long) : Id = LongId[Funder](i)
  private[models] val row = Columns(
      SelectColumn[Long]("fundref_id")
    , SelectColumn[String]("name")
    ).map { (id, name) =>
      new Funder(asId(id), name)
    }

  import play.api.libs.json
  import play.api.libs.ws

  private val fundRefDOI = "10.13039/"
  private def fundRefId(id : Id) =
    ws.WS.url("http://data.fundref.org/fundref/funder/" + fundRefDOI + id)
  private def fundRefSearch(query : String) =
    ws.WS.url("http://search.crossref.org/funders").withQueryString("q" -> query)
  private val geoNamesRes = "http://sws\\.geonames\\.org/([0-9]*)/".r
  private val geoNamesUS = "6252001"
  private def geoNamesId(id : String) =
    ws.WS.url("http://api.geonames.org/getJSON").withQueryString("geonameId" -> id, "username" -> "databrary")

  def apply(id : Id, name : String, aliases : Seq[String] = Nil, country : Option[String] = None) = {
    import org.apache.commons.lang3.StringUtils.containsIgnoreCase
    val (names, alts) = aliases.filterNot(containsIgnoreCase(name, _))
      .partition(containsIgnoreCase(_, name))
    val n = new StringBuilder(if (names.nonEmpty) names.maxBy(_.length) else name)
    if (alts.nonEmpty)
      n ++= alts.mkString(" (", ", ", ")")
    country.filterNot(c => c.equalsIgnoreCase("United States") || containsIgnoreCase(n.toString, c)).foreach { c =>
      n ++= ", "
      n ++= c
    }
    new Funder(id, n.toString)
  }

  private def getJson(r : ws.WSRequestHolder) : Future[Option[json.JsValue]] =
    r.get.map { r =>
      if (r.status == 200 && r.header("Content-Type").exists(_.startsWith("application/json")))
        Some(r.json)
      else
        None
    }

  private def fundLabel(j : json.JsValue) : Option[String] =
    (j \ "Label" \ "literalForm" \ "content").asOpt[String]
  private def fundrefId(id : Id) : Future[Option[Funder]] =
    getJson(fundRefId(id)).flatMap { r =>
      (for {
        j <- r
        doi <- (j \ "id").asOpt[String]
        id <- Maybe.toLong(doi.stripPrefix("http://dx.doi.org/" + fundRefDOI))
        name <- fundLabel(j \ "prefLabel")
      } yield ((j, id, name))).mapAsync { case (j, id, name) =>
        val altj = j \ "altLabel"
        val alts = altj.asOpt[json.JsArray].fold(Seq(altj))(_.value).flatMap(fundLabel(_))
        val country = (j \ "country" \ "resource").asOpt[String].flatMap {
          case geoNamesRes(r) if !r.equals(geoNamesUS) => Some(r)
          case _ => None
        }
        country.flatMapAsync(cid => getJson(geoNamesId(cid)).map(_.flatMap(_.\("name").asOpt[String])))
          .map(Funder(asId(id), name, alts, _))
      }
    }

  private[models] def get(id : Id) : Future[Option[Funder]] =
    row.SELECT(sql"WHERE fundref_id = $id").singleOpt.orElseAsync(
      fundrefId(id).filterAsync(f => INSERT(f.sqlArgs).execute))

  def search(query : String, all : Boolean = false) : Future[Seq[Funder]] =
    if (all)
      getJson(fundRefSearch(query)).map(_
        .flatMap(_.asOpt[json.JsArray])
        .fold[Seq[Funder]](Nil)(_.value.flatMap { j =>
          for {
            ids <- (j \ "id").asOpt[String]
            id <- Maybe.toLong(ids)
            name <- (j \ "value").asOpt[String]
            alts = (j \ "other_names").asOpt[json.JsArray].fold[Seq[String]](Nil)(_.value.flatMap(_.asOpt[String]))
            country = (j \ "country").asOpt[String]
          } yield (Funder(asId(id), name, alts, country))
        }))
    else
      row.SELECT(sql"WHERE name ILIKE ${"%"+query+"%"}").list
}

final case class Funding(val funder : Funder, val awards : IndexedSeq[String] = IndexedSeq.empty) {
  def json = JsonObject(
    'funder -> funder.json,
    'awards -> awards
  )
}

object VolumeFunding extends Table[Funding]("volume_funding") {
  private val row = Columns(
      SelectColumn[IndexedSeq[String]]("awards")
    ).join(Funder.row on "volume_funding.funder = funder.fundref_id")
    .map { case (awards, funder) =>
      new Funding(funder, awards)
    }

  private[models] def get(vol : Volume) : Future[Seq[Funding]] =
    row
    .SELECT(sql"WHERE volume = ${vol.id}")
    .list

  def set(vol : Volume, funderId : Funder.Id, awards : Option[IndexedSeq[String]]) : Future[Boolean] = {
    val ids = SQLTerms('volume -> vol.id, 'funder -> funderId)
    awards.fold(
      DELETE(ids).execute) { a =>
      Funder.get(funderId).flatMap(_.fold(async(false)) { _ =>
        val args = ('awards -> a) +: ids
        DBUtil.updateOrInsert(
          (lsql"UPDATE volume_funding SET awards = $a WHERE " ++ ids.where).run(_, _))(
          INSERT(args)(_, _)).execute
      })
    }
  }
}
