package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

class Annotation protected (val id : Annotation.Id, val whoId : Identity.Id, val when : Timestamp, val studyId : Study.Id, val objectId : Option[Object.Id]) extends TableRowId(id.unId) {
  protected[models] val _who = CachedVal[Identity, Site.DB](Identity.get(whoId)(_))
  def who(implicit db : Site.DB) : Identity = _who
  protected[models] val _study = CachedVal[Option[Study], Site](Study.get(studyId)(_))
  def study(implicit site : Site) : Option[Study] = _study
  protected[models] val _obj = CachedVal[Option[StudyObject], Site](s => objectId.flatMap(o => study(s).flatMap(_.getObject(o)(s.db))))
  def obj(implicit site : Site) : Option[StudyObject] = _obj
}

private[models] sealed abstract class AnnotationView[A <: Annotation](table : String) extends TableViewId[A](table) {
  private[models] def get(id : Id)(implicit db : Site.DB) : Option[A] =
    SQL("SELECT " + * + " FROM " + table + " WHERE id = {id}").
      on('id -> id).singleOpt(row)

  private[this] def getList(study  : Study.Id, obj : Option[Option[Object.Id]])(implicit db : Site.DB) : Seq[A] = obj.fold {
      SQL("SELECT " + * + " FROM " + table + " WHERE study = {study} ORDER BY id DESC").on('study -> study)
    } { _.fold {
      SQL("SELECT " + * + " FROM " + table + " WHERE study = {study} AND object IS NULL ORDER BY id DESC").on('study -> study)
    } { obj =>
      SQL("SELECT " + * + " FROM " + table + " WHERE study = {study} AND object = {obj} ORDER BY id DESC").on('study -> study, 'obj -> obj)
    } }.list(row)

  private[models] def getStudy(study : Study, only : Boolean = false)(implicit db : Site.DB) : Seq[A] =
    getList(study.id, if (only) Some(None) else None).map({ c =>
      c._study() = Some(study)
      c
    })
  private[models] def getStudyObject(obj : StudyObject)(implicit db : Site.DB) : Seq[A] =
    getList(obj.studyId, Some(Some(obj.objId))).map({ c =>
      c._obj() = Some(obj)
      c
    })
  /* This should ideally pull studies and check access as well */
  private[models] def getEntity(i : Identity)(implicit db : Site.DB) : Seq[A] =
    SQL("SELECT " + * + " FROM " + table + " WHERE who = {who} ORDER BY id DESC").
      on('who -> i.id).list(row).map({ c =>
        c._who() = i
        c
      })
}
private[models] object Annotation extends AnnotationView[Annotation]("annotation") {
  private[this] def make(id : Annotation.Id, whoId : Identity.Id, when : Timestamp, studyId : Study.Id, objectId : Option[Object.Id]) =
    new Annotation(id, whoId, when, studyId, objectId)
  private[models] val row = Anorm.rowMap(make _, "id", "who", "when", "study", "object")
}

final class Comment private (id : Annotation.Id, whoId : Identity.Id, when : Timestamp, studyId : Study.Id, objectId : Option[Object.Id], val text : String) extends Annotation(id, whoId, when, studyId, objectId) {
}

object Comment extends AnnotationView[Comment]("comment") {
  private[this] def make(id : Annotation.Id, whoId : Identity.Id, when : Timestamp, studyId : Study.Id, objectId : Option[Object.Id], text : String) =
    new Comment(id, whoId, when, studyId, objectId, text)
  private[models] val row = Anorm.rowMap(make _, "id", "who", "when", "study", "object", "text")

  private[this] def create(study : Study.Id, obj : Option[Object.Id], text : String)(implicit site : Site) = {
    val args = Anorm.Args('who -> site.identity.id, 'study -> study, 'object -> obj, 'text -> text)
    val c = SQL("INSERT INTO " + table + " " + Anorm.insertArgs(args) + " RETURNING " + *).
      on(args : _*).single(row)(site.db)
    c._who() = site.identity
    c
  }
  private[models] def create(study : Study, text : String)(implicit site : Site) : Comment = {
    val c = create(study.id, None, text)
    c._study() = Some(study)
    c
  }
  private[models] def create(obj : StudyObject, text : String)(implicit site : Site) : Comment = {
    val c = create(obj.studyId, Some(obj.objId), text)
    c._obj() = Some(obj)
    c
  }
}
