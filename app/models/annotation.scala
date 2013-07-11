package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

class Annotation protected (val id : Annotation.Id, val whoId : Identity.Id, val when : Timestamp, val studyId : Study.Id, val objectId : Option[Object.Id]) extends TableRowId(id.unId) {
  protected val _who = CachedVal[Identity, Site.DB](Identity.get(whoId)(_))
  def who(implicit db : Site.DB) : Identity = _who
  protected val _study = CachedVal[Study, Site](Study.get(studyId)(_).get)
  def study(implicit site : Site) : Study = _study
  protected val _obj = CachedVal[Option[StudyObject], Site.DB](db => objectId.map(StudyObject.get(studyId, _)(db).get))
  def obj(implicit db : Site.DB) : Option[StudyObject] = _obj
}

private[models] object Annotation extends TableViewId[Annotation]("annotation") {
  private[this] def make(id : Annotation.Id, whoId : Identity.Id, when : Timestamp, studyId : Study.Id, objectId : Option[Object.Id]) =
    new Annotation(id, whoId, when, studyId, objectId)
  private[models] val row = Anorm.rowMap(make _, "id", "who", "when", "study", "object")
}

final class Comment private (id : Annotation.Id, whoId : Identity.Id, when : Timestamp, studyId : Study.Id, objectId : Option[Object.Id], val text : String, val replyToId : Option[Comment.Id]) extends Annotation(id, whoId, when, studyId, objectId) {
}

object Comment extends TableViewId[Comment]("comment") {
  private[this] def make(id : Annotation.Id, whoId : Identity.Id, when : Timestamp, studyId : Study.Id, objectId : Option[Object.Id], text : String, replyToId : Option[Comment.Id]) =
    new Comment(id, whoId, when, studyId, objectId, text, replyToId)
  private[models] val row = Anorm.rowMap(make _, "id", "who", "when", "study", "object", "text", "reply_to")

  private[models] def get(id : Id)(implicit db : Site.DB) : Option[Comment] =
    SQL("SELECT " + * + " FROM " + table + " WHERE id = {id}").
      on('id -> id).singleOpt(row)

  private[this] def get(study  : Study.Id, obj : Option[Option[Object.Id]])(implicit db : Site.DB) : Seq[Comment] = obj.fold {
      SQL("SELECT " + * + " FROM " + table + " WHERE study = {study}").on('study -> study)
    } { _.fold {
      SQL("SELECT " + * + " FROM " + table + " WHERE study = {study} AND object IS NULL").on('study -> study)
    } { obj =>
      SQL("SELECT " + * + " FROM " + table + " WHERE study = {study} AND object = ?").on('study -> study, 'object -> obj)
    } }.list(row)

  private[models] def get(study : Study, only : Boolean)(implicit db : Site.DB) : Seq[Comment] =
    get(study.id, if (only) Some(None) else None).map({ c =>
      c._study() = study
      c
    })
  private[models] def get(obj : StudyObject)(implicit db : Site.DB) : Seq[Comment] =
    get(obj.studyId, Some(Some(obj.objId))).map({ c =>
      c._obj() = Some(obj)
      c
    })

  private[this] def create(study : Study.Id, obj : Option[Object.Id], text : String, replyTo : Option[Comment.Id])(implicit site : Site) = {
    val args = Anorm.Args('who -> site.identity.id, 'study -> study, 'object -> obj, 'text -> text, 'reply_to -> replyTo)
    val c = SQL("INSERT INTO " + table + " " + Anorm.insertArgs(args) + " RETURNING " + *).
      on(args : _*).single(row)(site.db)
    c._who() = site.identity
    c
  }
  private[models] def create(study : Study, text : String, replyTo : Option[Comment.Id])(implicit site : Site) : Comment = {
    val c = create(study.id, None, text, replyTo)
    c._study() = study
    c
  }
  private[models] def create(obj : StudyObject, text : String, replyTo : Option[Comment.Id])(implicit site : Site) : Comment = {
    val c = create(obj.studyId, Some(obj.objId), text, replyTo)
    c._obj() = Some(obj)
    c
  }
}
