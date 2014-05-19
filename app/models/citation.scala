package models

import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import macros.async._
import dbrary._
import site._

/** A citation or reference to a publication or other external resource associated with a Volume.
  * This interface is temporary, quick and dirty, just to provide minimal functionality, and should not be expected to remain as is. */
final case class Citation(val head : String, val url : Option[java.net.URL], val body : Option[String], val study : Boolean = false) extends TableRow {
  private[models] def sqlKey = SQLTerms()
  private[models] def sqlArgs = SQLTerms('head -> head, 'url -> url.map(_.toString), 'body -> body)

  def lookup : Future[Citation] =
    url.flatMapAsync(Cite.getBibliography(_))
    .map(_.fold(this)(h => copy(head = h)))

  def json = JsonObject.flatten(
    Some('head -> head),
    url.map(u => ('url, u.toString)),
    body.map('body -> _)
  )
}

object VolumeCitation extends Table[Citation]("volume_citation") {
  private val columns = Columns(
      SelectColumn[String]("head")
    , SelectColumn[Option[String]]("url")
    , SelectColumn[Option[String]]("body")
    , SelectColumn[Boolean]("study")
    ).map { (head, url, body, study) =>
      new Citation(head, url.flatMap(dbrary.url.parse _), body, study)
    }

  private[models] def getVolume(vol : Volume) : Future[Seq[Citation]] =
    columns.SELECT("WHERE volume = ? ORDER BY study DESC, head").apply(vol.id).list

  private def set(vol : Volume, list : TraversableOnce[Citation], study : Boolean) : Future[Unit] = {
    val i = SQLTerms('volume -> vol.id, 'study -> study)
    val dbc = implicitly[Site.DB]
    val exc = implicitly[ExecutionContext]
    dbc.inTransaction { dbc => for {
      _ <- DELETE(i)(dbc, exc)
      _ <- list.foreachAsync(c =>
	INSERT(i ++ c.sqlArgs)(dbc, exc).execute)
    } yield () }
  }

  private[models] def setVolume(vol : Volume, list : Seq[Citation]) : Future[Unit] =
    set(vol, list, false)

  private[models] def setVolumeStudy(vol : Volume, cite : Option[Citation]) : Future[Unit] =
    set(vol, cite, true)
}
