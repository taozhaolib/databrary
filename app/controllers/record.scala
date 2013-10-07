package controllers

import util._
import play.api._
import          templates.Html
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import          libs.json._
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
          "id" -> Json.toJson(c.id.unId.toString),
          "name" -> Json.toJson(c.name),
          "template" -> Json.toJson(c.template.map {
            case m =>
              Json.toJson(Map(
                "id" -> Json.toJson(m.id.unId.toString),
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

  private def editFormFill(implicit request : Request[_]) : (Seq[Metric],  EditForm) = {
    val r = request.obj
    val m = r.measures
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

  def update(v : models.Volume.Id, i : models.Record.Id) = Action(v, i, Permission.EDIT) { implicit request =>
    val (meas, formin) = editFormFill
    val form = formin.bindFromRequest
    form.fold(
      form => BadRequest(views.html.record.edit(request.obj, meas, form, js)),
      { case (category, data) =>
        request.obj.change(category = category.flatMap(RecordCategory.get(_)))
        def update(metric : Metric.Id, datum : Option[String]) : Option[String] =
          Metric.get(metric).fold[Option[String]](Some("measure.unknown")) { m =>
            datum.fold {
              request.obj.deleteMeasure(m)
              None : Option[String]
            } { value =>
              if (!request.obj.setMeasure(m, value))
                Some("measure.bad")
              else
                None
            }
          }
        data.zipWithIndex.foldLeft {
          form
        } { (form, measure) =>
          val ((metric, datum), i) = measure
          update(metric, datum).fold(form)(form.withError("measure.datum[" + i + "]", _))
        }.fold(
          form => BadRequest(views.html.record.edit(request.obj, meas, form, js)),
          _ => Redirect(request.obj.pageURL)
        )
      }
    )
  }

  type SelectForm = Form[Option[models.Record.Id]]
  protected[controllers] val selectForm = Form(
    "record" -> optional(of[models.Record.Id])
  )

  def selectList(target : Slot)(implicit request : SiteRequest[_]) : Seq[(String, String)] = {
    /* ideally we'd remove already used records here */
    target.volume.allRecords() map { r : Record =>
      (r.id.unId.toString, r.category.fold("")(_.name + ':') + r.ident.getOrElse("[" + r.id.unId.toString + "]"))
    }
  }

  def slotRemove(v : models.Volume.Id, s : models.Slot.Id, r : models.Record.Id, editRedirect : Boolean = false) = Slot.Action(v, s, Permission.EDIT) { implicit request =>
    request.obj.removeRecord(r)
    if (editRedirect)
      Redirect(routes.Slot.edit(v, s))
    else
      Redirect(request.obj.pageURL)
  }

  def slotAdd(v : models.Volume.Id, s : models.Slot.Id, editRedirect : Boolean = false) = Slot.Action(v, s, Permission.EDIT) { implicit request =>
    val form = selectForm.bindFromRequest
    form.fold(
      form => BadRequest(Slot.viewEdit(request.obj)(recordForm = form)),
      _.fold[SimpleResult] {
        val r = models.Record.create(request.obj.volume)
        r.addSlot(request.obj)
        Created(views.html.record.edit(r, Nil, editForm, js))
      } (models.Record.get(_).
        filter(r => r.permission >= Permission.DOWNLOAD && r.volumeId == v).
        fold[SimpleResult](
          BadRequest(Slot.viewEdit(request.obj)(recordForm = form.withError("record", "record.bad")))) { r =>
          r.addSlot(request.obj)
          if (editRedirect)
            Redirect(routes.Slot.edit(v, s))
          else
            Redirect(request.obj.pageURL)
        }
      )
    )
  }

}
