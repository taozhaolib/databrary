package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

sealed abstract class Annotation protected (val id : Annotation.Id) extends TableRowId[Annotation] {
  /* this checks permissions */
  def containers(implicit site : Site) : Seq[Container] = Container.getAnnotation(this)(site)
}

final class Comment private (override val id : Comment.Id, val whoId : Party.Id, val when : Timestamp, val text : String) extends Annotation(id) with TableRowId[Comment] {
  def companion = Comment
  protected[models] val _who = CachedVal[Party, Site](Party.get(whoId)(_).get)
  def who(implicit site : Site) : Party = _who
}


private[models] sealed abstract class AnnotationView[R <: Annotation with TableRowId[R]](table : String) extends TableId[R](table) {
  private[models] def get(id : Id)(implicit db : Site.DB) : Option[R] =
    SELECT("WHERE id = {id}").
      on('id -> id).singleOpt()

  private[models] def get(a : Annotated, all : Boolean = true)(implicit db : Site.DB) : Seq[R] = {
    val j = a.annotatedLevel + "_annotation"
    SELECT(if (all) 
        "JOIN " + j + "s({a}) ON " + table + ".id = " + j + "s"
      else
        "JOIN " + j + " ON " + table + ".id = annotation WHERE " + a.annotatedLevel + " = {a}").
      on('a -> a.annotatedId).list()
  }
}

object Annotation extends HasId[Annotation] //AnnotationView[Annotation]("annotation")

object Comment extends AnnotationView[Comment]("comment") {
  private[models] val row = Columns[
    Id,  Party.Id, Timestamp, String](
    'id, 'who,     'when,     'text).map {
    (id, whoId, when, text) => new Comment(id, whoId, when, text)
  }

  private[models] def getParty(i : Party)(implicit db : Site.DB) : Seq[Comment] =
    SELECT("WHERE who = {who}").
      on('who -> i.id).list(row map { a =>
        a._who() = i
        a
      })

  private[models] def post(a : Annotated, text : String)(implicit site : Site) : Comment = {
    val args = SQLArgs('who -> site.identity.id, 'text -> text)
    val c = SQL("INSERT INTO " + table + " " + args.insert + " RETURNING " + *).
      on(args : _*).single(row)(site.db)
    c._who() = site.identity
    Audit.add(a.annotatedLevel + "_annotation", SQLArgs(Symbol(a.annotatedLevel) -> a.annotatedId, 'annotation -> c.id)).
      execute()(site.db)
    c
  }
}


trait Annotated {
  private[models] def annotatedId : IntId[_]
  private[models] def annotatedLevel : String
  def comments(all : Boolean = true)(implicit db : Site.DB) : Seq[Comment] = Comment.get(this, all)(db)
  def postComment(text : String)(implicit site : Site) : Comment = Comment.post(this, text)(site)
}
