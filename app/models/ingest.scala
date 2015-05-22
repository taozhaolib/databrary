package models

import scala.concurrent.{ExecutionContext,Future}
import dbrary._
import dbrary.SQL._

object Ingest {
  def getContainer(v : Volume, key : String)(implicit dbc : site.Site.DB, exc : ExecutionContext) : Future[Option[Container]] =
    models.Container.rowVolume(v)
    .SELECT(lsql"JOIN ingest.container AS ingest ON container.id = ingest.id AND container.volume = ingest.volume WHERE key = $key")
    .singleOpt

  def setContainer(container : Container, key : String)(implicit dbc : site.Site.DB, exc : ExecutionContext) : Future[Unit] =
    lsql"INSERT INTO ingest.container (id, volume, key) VALUES (${container.id}, ${container.volumeId}, $key)"
    .run.ensure

  def getRecord(v : Volume, key : String)(implicit dbc : site.Site.DB, exc : ExecutionContext) : Future[Option[Record]] =
    models.Record.rowVolume(v)
    .SELECT(lsql"JOIN ingest.record AS ingest ON record.id = ingest.id AND record.volume = ingest.volume WHERE key = $key")
    .singleOpt

  def setRecord(record : Record, key : String)(implicit dbc : site.Site.DB, exc : ExecutionContext) : Future[Unit] =
    lsql"INSERT INTO ingest.record (id, volume, key) VALUES (${record.id}, ${record.volumeId}, $key)"
    .run.ensure

  def getAsset(v : Volume, path : String)(implicit dbc : site.Site.DB, exc : ExecutionContext) : Future[Option[Asset]] =
    models.Asset.rowVolume(v)
    .SELECT(lsql"JOIN ingest.asset AS ingest ON asset.id = ingest.id WHERE file = $path")
    .singleOpt

  def getAssetClip(o : Asset, clip : Segment)(implicit dbc : site.Site.DB, exc : ExecutionContext) : Future[Option[Asset]] =
    models.Asset.rowVolume(o.volume)
    .SELECT(lsql"JOIN transcode ON asset.id = transcode.asset WHERE transcode.orig = ${o.id} AND segment = $clip")
    .singleOpt

  def setAsset(asset : Asset, path : String)(implicit dbc : site.Site.DB, exc : ExecutionContext) : Future[Unit] =
    lsql"INSERT INTO ingest.asset (id, file) VALUES (${asset.id}, $path)"
    .run.ensure
}
