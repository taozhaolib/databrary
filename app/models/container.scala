package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

sealed abstract class Container protected (val id : Container.Id) extends TableRowId(id.unId) with SitePage {
  /* Study owning this container (possibly itself) */
  def study : Study

  def objects(implicit db : Site.DB) = ObjectLink.getObjects(this)
  def getObject(o : Object.Id)(implicit db : Site.DB) = ObjectLink.get(this, o)

  def comments(only : Boolean = false)(implicit db : Site.DB) = Comment.getContainer(this, only)(db)
  def addComment(text : String)(implicit site : Site) = Comment.create(this, text)
}

final class Study private (id : Study.Id, title_ : String, description_ : Option[String], val permission : Permission.Value) extends Container(id) {
  def study = this

  private[this] var _title = title_
  def title = _title
  private[this] var _description = description_
  def description = _description

  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    val args = Anorm.Args('id -> id, 'title -> title, 'description -> description)
    Audit.SQLon(AuditAction.change, "study", "SET title = {title}, description = {description} WHERE id = {id}")(args : _*).execute()(site.db)
    _title = title
    _description = description
  }

  def pageName(implicit site : Site) = title
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Study.view(id).url

  def entityAccess(p : Permission.Value = Permission.NONE)(implicit db : Site.DB) = StudyAccess.getEntities(this, p)

  def slots(implicit db : Site.DB) = Slot.getStudy(this)
}

final class Slot private (id : Slot.Id, val study : Study, ident_ : String) extends Container(id) {
  def studyId = study.id
  private[this] var _ident = ident_
  def ident = _ident

  def change(ident : String = _ident)(implicit site : Site) : Unit = {
    if (ident == _ident)
      return
    /* TODO: catch unique violation */
    Audit.SQLon(AuditAction.change, "slot", "SET ident = {ident} WHERE id = {id}")('id -> id, 'ident -> ident).execute()(site.db)
    _ident = ident
  }

  def pageName(implicit site : Site) = ident
  def pageParent(implicit site : Site) = Some(study)
  def pageURL = ???
}


/* It would be nice if Study.Id strictly <: Container.Id, but the current NewId thing doesn't allow it without a lot of boilerplate */
private[models] object ContainerId extends NewId

private[models] sealed abstract class ContainerView[R <: Container](table : String) extends TableView[R](table) with HasId {
  type Id = ContainerId.Id
  def asId(i : Int) : Id = ContainerId.asId(i)

  protected final val permission = "study_access_check(study.id, {identity})"
  private[models] final val condition = permission + " >= 'VIEW'"

  def get(i : Id)(implicit site : Site) : Option[R]
}

private[models] object Container extends ContainerView[Container]("container") {
  private[models] val row =
    (Study.row ~ Slot.baseRow.?) map {
      case (study ~ None) => study
      case (study ~ Some(slot)) => Slot.baseMake(slot, study)
    }
  private[models] override val * = Study.* + ", " + Slot.*
  private[models] override val src = "container LEFT JOIN slot USING (id) JOIN study ON study.id = container.id OR study.id = slot.study"
  def get(i : Id)(implicit site : Site) : Option[Container] =
    SQL("SELECT " + * + " FROM " + src + " WHERE id = {id} AND " + condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt(row)(site.db)
}

object Study extends ContainerView[Study]("study") {
  private[this] def make(id : Id, title : String, description : Option[String], permission : Option[Permission.Value]) =
    new Study(id, title, description, permission.getOrElse(Permission.NONE))
  private[models] val row = Anorm.rowMap(make _, col("id"), col("title"), col("description"), "permission")
  private[models] override val * = col("*") + ", " + permission + " AS permission"

  def get(i : Id)(implicit site : Site) : Option[Study] =
    SQL("SELECT " + * + " FROM study WHERE id = {id} AND " + condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt(row)(site.db)
    
  def create(title : String, description : Option[String] = None)(implicit site : Site) : Study = {
    val args = Anorm.Args('title -> title, 'description -> description)
    Audit.SQLon(AuditAction.add, "study", Anorm.insertArgs(args), *)(args : _*).single(row)(site.db)
  }
}

object Slot extends ContainerView[Slot]("slot") {
  private[models] val baseRow = Anorm.rowMap(Tuple2.apply[Id, String] _, col("id"), col("ident"))
  private[models] def baseMake(ii : (Id, String), study : Study) = new Slot(ii._1, study, ii._2)
  private[models] override val * = col("id", "ident")

  private[models] val row = 
    (baseRow ~ Study.row) map {
      case (slot ~ study) => baseMake(slot, study)
    }
  private[models] override val src = "slot JOIN study ON slot.study = study.id"
  private[this] def rowStudy(study : Study) =
    baseRow map { slot => baseMake(slot, study) }

  def get(i : Id)(implicit site : Site) : Option[Slot] =
    SQL("SELECT " + * + ", " + Study.* + " FROM " + src + " WHERE slot.id = {id} AND " + condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt(row)(site.db)
  private[models] def getStudy(study : Study)(implicit db : Site.DB) : Seq[Slot] =
    SQL("SELECT " + * + " FROM slot WHERE study = {study} ORDER BY ident").
      on('study -> study.id).list(rowStudy(study))
    
  private[models] def create(study : Study, ident : String)(implicit site : Site) : Slot = {
    val args = Anorm.Args('study -> study, 'ident -> ident)
    Audit.SQLon(AuditAction.add, table, Anorm.insertArgs(args), *)(args : _*).single(rowStudy(study))(site.db)
  }
}

