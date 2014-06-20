package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import macros.async._
import dbrary._
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
  import play.api.libs.ws.WS

  private val fundRefDOI = "10.13039/"
  private val fundRefId = "http://data.fundref.org/fundref/funder/" + fundRefDOI
  private val fundRefSearch = WS.url("http://search.crossref.org/funders")

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

  private def fundLabel(j : json.JsValue) : Option[String] =
    (j \ "Label" \ "literalForm" \ "content").asOpt[String]
  private def fundrefId(id : Id) : Future[Option[Funder]] =
    WS.url(fundRefId + id)
    .get.map { r =>
      for {
	ct <- r.header("Content-Type")
	if r.status == 200 && ct.startsWith("application/json")
	j = r.json
	doi <- (j \ "id").asOpt[String]
	id <- Maybe.toLong(doi.stripPrefix("http://dx.doi.org/" + fundRefDOI))
	name <- fundLabel(j \ "prefLabel")
	alts = (j \ "altLabel").asOpt[json.JsArray].fold[Seq[String]](Nil)(_.value.flatMap(fundLabel(_)))
	country = (j \ "country").asOpt[String]
      } yield (Funder(asId(id), name, alts, country))
    }

  private[models] def get(id : Id) : Future[Option[Funder]] =
    row.SELECT("WHERE id = ?").apply(id).singleOpt.flatMap(_.orElseAsync(
      fundrefId(id).andThen {
	case scala.util.Success(Some(f)) => INSERT(f.sqlArgs)
      }))

  def search(query : String, all : Boolean = false) : Future[Seq[Funder]] =
    if (all)
      fundRefSearch.withQueryString("q" -> query)
      .get.map { r =>
	(for {
	  ct <- r.header("Content-Type")
	  if r.status == 200 && ct.startsWith("application/json")
	  j <- r.json.asOpt[json.JsArray]
	} yield j.value.flatMap { j =>
	  for {
	    ids <- (j \ "id").asOpt[String]
	    id <- Maybe.toLong(ids)
	    name <- (j \ "value").asOpt[String]
	    alts = (j \ "other_names").asOpt[json.JsArray].fold[Seq[String]](Nil)(_.value.flatMap(_.asOpt[String]))
	    country = (j \ "country").asOpt[String]
	  } yield (Funder(asId(id), name, alts, country))
	}).getOrElse(Nil)
      }
    else
      row.SELECT("WHERE name ILIKE ?").apply("%" + query + "%").list
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
    ).join(Funder.row, "volume_funding.funder = funder.fundref_id")
    .map { case (awards, funder) =>
      new Funding(funder, awards)
    }

  private[models] def get(vol : Volume) : Future[Seq[Funding]] =
    row
    .SELECT("WHERE volume = ?")
    .apply(vol.id).list

  def set(vol : Volume, funderId : Funder.Id, awards : Option[IndexedSeq[String]]) : Future[Boolean] = {
    val ids = SQLTerms('volume -> vol.id, 'funder -> funderId)
    awards.fold(
      DELETE(ids).execute) { a =>
      Funder.get(funderId).flatMap(_.fold(async(false)) { _ =>
	val args = ('awards -> a) +: ids
	DBUtil.updateOrInsert(
	  SQL("UPDATE volume_funding SET awards = ? WHERE", ids.where)(_, _).apply(args))(
	  INSERT(args)(_, _)).execute
      })
    }
  }
}
