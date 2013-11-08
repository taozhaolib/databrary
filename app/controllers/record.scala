package controllers

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api._
import          templates.Html
import          Play.current
import          mvc._
import          data._
import               Forms._
import          libs.json._
import site._
import models._

object Record extends SiteController {
  type Request[A] = RequestObject[Record]#Site[A]

  private[controllers] def action(v : models.Volume.Id, i : models.Record.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(v, models.Record.get(i)(_), p)

  private[controllers] def Action(v : models.Volume.Id, i : models.Record.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction ~> action(v, i, p)

  def view(v : models.Volume.Id, i : models.Record.Id) = Action(v, i) { implicit request =>
    Ok(views.html.record.view(request.obj))
  }

  private type MeasureMapping = (Metric.Id, Option[String])
  private val measureMapping = tuple(
    "metric" -> of[Metric.Id],
    "datum" -> optional(nonEmptyText)
  )
  type EditForm = Form[(Option[RecordCategory.Id], Seq[MeasureMapping])]
  private val editForm = Form(tuple(
    "category" -> optional(of[RecordCategory.Id]),
    "measure" -> seq(measureMapping)
  ))

  private val js = {
    Html(Json.stringify(Json.toJson(RecordCategory.getAll.map {
      case c =>
        Json.toJson(Map(
          "id" -> Json.toJson(c.id.toString),
          "name" -> Json.toJson(c.name),
          "template" -> Json.toJson(c.template.map {
            case m =>
              Json.toJson(Map(
                "id" -> Json.toJson(m.id.toString),
                "name" -> Json.toJson(m.name),
                "dataType" -> Json.toJson(m.dataType.toString),
                "classification" -> Json.toJson(m.classification.toString),
                "values" -> Json.toJson(m.values)
              ))
          })
        ))
    }
    )))
  }

  private def editFormFill(implicit request : Request[_]) : (Seq[Metric], EditForm) = {
    val r = request.obj
    val m = macros.Async.get(r.measures)
    val mm = m.map(_.metric)
    val t = r.category.fold[Seq[Metric]](Nil)(_.template).diff(mm)
    (mm ++ t, editForm.fill(
      (
        r.categoryId,
        m.map(m => (m.metricId, Some(m.datum.toString))) ++ t.map(_.id -> None)
      )
    ))
  }

  def edit(v : models.Volume.Id, i : models.Record.Id) = Action(v, i, Permission.EDIT) { implicit request =>
    val (m, f) = editFormFill
    Ok(views.html.record.edit(request.obj, m, f, js))
  }

  def update(v : models.Volume.Id, i : models.Record.Id) = Action(v, i, Permission.EDIT).async { implicit request =>
    val (meas, formin) = editFormFill
    val form = formin.bindFromRequest
    form.fold(
      form => ABadRequest(views.html.record.edit(request.obj, meas, form, js)),
      { case (category, data) =>
        request.obj.change(category = category.flatMap(RecordCategory.get(_))).flatMap { _ =>
        val filled = scala.collection.mutable.Set.empty[Int] // temporary hack to prevent data corruption with duplicate metrics
        def update(metric : Metric.Id, datum : Option[String]) : Future[Option[String]] =
          Metric.get(metric).flatMap(_.fold[Future[Option[String]]](
            macros.Async(Some("measure.unknown"))
          ) { m =>
            datum.fold[Future[Option[String]]] {
              if (!filled.contains(metric.unId))
                request.obj.deleteMeasure(m).map(_ => None)
              else
                macros.Async(None)
            } { value =>
              filled.add(metric.unId)
              request.obj.setMeasure(m, value).map {
                case false => Some("measure.bad")
                case true => None
              }
            }
          })
        macros.Async.sequence(data.map((update _).tupled)).map {
        _.zipWithIndex.foldLeft(form) { (form, error) => error match {
          case (None, _) => form
          case (Some(error), i) => form.withError("measure.datum[" + i + "]", error)
        } }.fold(
          form => BadRequest(views.html.record.edit(request.obj, meas, form, js)),
          _ => Redirect(request.obj.pageURL)
        )
        } }
      }
    )
  }

  type SelectForm = Form[Option[models.Record.Id]]
  protected[controllers] val selectForm = Form(
    "record" -> optional(of[models.Record.Id])
  )

  private[controllers] def selectList(target : Slot)(implicit request : SiteRequest[_]) : Future[Seq[(String, String)]] =
    /* ideally we'd remove already used records here */
    target.volume.allRecords().map(_ map { r : Record =>
      (r.id.toString, r.category.fold("")(_.name + ':') + macros.Async.get(r.ident).getOrElse("[" + r.id.toString + "]"))
    })

  def slotRemove(v : models.Volume.Id, s : models.Slot.Id, r : models.Record.Id, editRedirect : Boolean = false) = Slot.Action(v, s, Permission.EDIT) { implicit request =>
    request.obj.removeRecord(r)
    if (editRedirect)
      Redirect(routes.Slot.edit(v, s))
    else
      Redirect(request.obj.pageURL)
  }

  def slotAdd(v : models.Volume.Id, s : models.Slot.Id, catID : models.RecordCategory.Id, editRedirect : Boolean = false) = Slot.Action(v, s, Permission.EDIT).async { implicit request =>
    val form = selectForm.bindFromRequest
    form.fold(
      form => Slot.viewEdit(Slot.BadRequest, request.obj)(recordForm = form),
      _.fold {
        val cat = RecordCategory.get(catID)
        for {
          r <- models.Record.create(request.obj.volume, cat)
          _ <- r.addSlot(request.obj)
        } yield (Created(views.html.record.edit(r, cat.fold[Seq[Metric]](Nil)(_.template), editForm.fill((cat.map(_.id), Seq())), js)))
      } (models.Record.get(_).flatMap(_
        .filter(r => r.checkPermission(Permission.DOWNLOAD) && r.volumeId == v)
        .fold(
          Slot.viewEdit(Slot.BadRequest, request.obj)(recordForm = form.withError("record", "record.bad"))
        ) { r => r.addSlot(request.obj).map { _ =>
          if (editRedirect)
            Redirect(routes.Slot.edit(v, s))
          else
            Redirect(request.obj.pageURL)
        } }
      ))
    )
  }

  def add(v : models.Volume.Id, catID : models.RecordCategory.Id) = Volume.Action(v, Permission.EDIT).async { implicit request =>
    val cat = RecordCategory.get(catID)
    models.Record.create(request.obj.volume, cat).map { r =>
      Created(views.html.record.edit(r, cat.fold[Seq[Metric]](Nil)(_.template), editForm.fill((cat.map(_.id), Seq())), js))
    }
  }

}
