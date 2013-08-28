package models

import java.sql.Timestamp
import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

/** One of a variety of types of metadata that may be attached to other objects in the system.
  * Currently annotations can be placed on any [[Container]]s and [[Asset]]s, as determined by the [[Annotated]] trait.
  * Permissions over annotations are generally determined by the permissions on the objects to which they attach.
  * The exact permissions and ownership semantics depend on the particular asset type. */
sealed abstract class Annotation protected (val id : Annotation.Id) extends TableRowId[Annotation] {
  /** The set of containers to which this annotation applies.
    * This checks permissions, so a non-empty list implies the annotation is visible to the current user. */
  def containers(implicit site : Site) : Seq[Container] = Container.getAnnotation(this)(site)
  /** The set of assets to which this annotation applies.
    * This does not check permissions, so must be followed by additional checks such as `assets.flatMap(_.containers)` to ensure the annotation (and asset) may be accessed. */
  def assets(implicit db : Site.DB) : Seq[Asset] = Asset.getAnnotation(this)(db)

  /** The set of all containers to which this asset applies, directly or indirectly through contained assets or clips of those assets.
    * This does check permissions, but is not necessarily very useful, because it does not indicate exactly which objects the annotation is applied to. */
  def allContainers(implicit site : Site) : Seq[Container] = 
    containers(site) ++ assets(site.db).flatMap(_.containers(true)(site).map(_.container(site)))
}

/** A comment made by a particular user, usually only applied to exactly one object.
  * These are immutable (and unaudited), although the author may be considered to have ownership. */
final class Comment private (override val id : Comment.Id, val whoId : Account.Id, val when : Timestamp, val text : String) extends Annotation(id) with TableRowId[Comment] {
  protected[models] val _who = CachedVal[Account, Site](Account.get(whoId)(_).get)
  /** The user who made the comment. */
  def who(implicit site : Site) : Account = _who
}


private[models] sealed abstract class AnnotationView[R <: Annotation with TableRowId[R]](table : String) extends TableId[R](table) {
  /** Retrieve a specific annotation of the instantiated object's type by id. */
  private[models] def get(id : Id)(implicit db : Site.DB) : Option[R] =
    SELECT("WHERE id = {id}").
      on('id -> id).singleOpt()

  /** Retrieve the set of annotations of the instantiated object's type on the given target.
    * @param all include all indirect annotations on any containers, objects, or clips contained within the given target (which may be a lot) */
  private[models] def get(target : Annotated, all : Boolean = true)(implicit db : Site.DB) : Seq[R] = {
    val j = target.annotatedLevel + "_annotation"
    SELECT(if (all) 
        "JOIN " + j + "s({target}) ON " + table + ".id = " + j + "s"
      else
        "JOIN " + j + " ON " + table + ".id = annotation WHERE " + target.annotatedLevel + " = {target}").
      on('target -> target.annotatedId).list()
  }
}

/** Dummy object for providing generic [[Annotation.Id]]s */
object Annotation extends HasId[Annotation] //AnnotationView[Annotation]("annotation")

object Comment extends AnnotationView[Comment]("comment") {
  private[models] val row = Columns[
    Id,  Account.Id, Timestamp, String](
    'id, 'who,       'when,     'text).map {
    (id, whoId, when, text) => new Comment(id, whoId, when, text)
  }

  /** Retrieve the set of comments written by the specified user. */
  private[models] def getParty(user : Account)(implicit db : Site.DB) : Seq[Comment] =
    SELECT("WHERE who = {who}").
      on('who -> user.id).list(row map { a =>
        a._who() = user
        a
      })

  /** Post a new comment on a target by the current user.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  private[models] def post(target : Annotated, text : String)(implicit site : Site) : Comment = {
    val args = SQLArgs('who -> site.identity.id, 'text -> text)
    val c = SQL("INSERT INTO " + table + " " + args.insert + " RETURNING " + *).
      on(args : _*).single(row)(site.db)
    c._who() = site.user.get
    Audit.add(target.annotatedLevel + "_annotation", SQLArgs(Symbol(target.annotatedLevel) -> target.annotatedId, 'annotation -> c.id)).
      execute()(site.db)
    c
  }
}


/** Objects on which annotations may be placed. */
trait Annotated {
  private[models] def annotatedId : IntId[_]
  private[models] def annotatedLevel : String
  /** The list of comments on this object.
    * @param all include indirect comments on any contained objects
    */
  def comments(all : Boolean = true)(implicit db : Site.DB) : Seq[Comment] = Comment.get(this, all)(db)
  /** Post a new comment this object.
    * This will throw an exception if there is no current user, but does not check permissions otherwise. */
  def postComment(text : String)(implicit site : Site) : Comment = Comment.post(this, text)(site)
}
