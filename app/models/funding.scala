package models

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import macros.async._
import dbrary._
import site._

final case class Funder(val id : Funder.Id, val name : String) extends TableRow {
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

  private val fundRefDOI = "10.13039/"
  private val fundRefId = "http://data.fundref.org/fundref/funder/" + fundRefDOI

  private def fundrefId(id : Id) : Future[Option[Funder]] =
    play.api.libs.ws.WS.url(fundRefId + id)
    .get.map { r =>
      for {
	ct <- r.header("Content-Type")
	if r.status == 200 && ct.startsWith("application/json")
	j = r.json
	doi <- (j \ "id").asOpt[String]
	id <- Maybe.toLong(doi.stripPrefix("http://dx.doi.org/" + fundRefDOI))
	name <- (j \ "prefLabel" \ "Label" \ "literalForm" \ "content").asOpt[String]
      } yield (new Funder(asId(id), name))
    }

  private[models] def get(id : Id) : Future[Option[Funder]] =
    row.SELECT("WHERE id = ?").apply(id).singleOpt.flatMap(_.orElseAsync(
      fundrefId(id).andThen {
	case scala.util.Success(Some(f)) => INSERT(f.sqlArgs)
      }))

  def search(query : String) : Future[Seq[Funder]] =
    row.SELECT("WHERE name ILIKE ?").apply("%" + query + "%").list
}

final case class Funding(val funder : Funder, val awards : IndexedSeq[String] = IndexedSeq.empty) {
  def json = JsonObject(
    'funder -> funder.name,
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
