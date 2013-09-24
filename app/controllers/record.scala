package controllers

import util._
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import models._

object Record extends SiteController {

  private[controllers] def check(i : models.Record.Id, p : Permission.Value = Permission.VIEW)(act : Record => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    models.Record.get(i).fold(NotFound : Result) { record =>
      if (record.permission < p)
        Forbidden
      else
        act(record)(request)
    }
  }

  def view(v : models.Volume.Id, i : models.Record.Id) = check(i) { record => implicit request =>
    Ok(views.html.record.view(record))
  }

  private type MeasureMapping = (Metric.Id, Option[String])
  private val measureMapping = tuple(
    "metric" -> of[Metric.Id],
    "datum" -> optional(nonEmptyText)
  )
  type EditForm = Form[(Option[RecordCategory.Id], Seq[MeasureMapping], MeasureMapping)]
  private val editForm = Form(tuple(
    "category" -> optional(of[RecordCategory.Id]),
    "measure" -> seq(measureMapping),
    "add" -> measureMapping
  ))

  private def editFormFill(r : Record)(implicit site : Site) : (Seq[Metric], EditForm) = {
    val m = r.measures
    val mm = m.map(_.metric)
    val t = r.category.fold(Nil : Seq[Metric])(_.template).diff(mm)
    (mm ++ t, editForm.fill((
      r.categoryId,
      m.map(m => (m.metricId, Some(m.datum.toString))) ++ t.map(m => (m.id, None)),
      (Metric.asId(0), None)
    )))
  }

  def edit(v : models.Volume.Id, i : models.Record.Id) = check(i, Permission.EDIT) { record => implicit request =>
    val (m, f) = editFormFill(record)
    Ok(views.html.record.edit(record, m, f))
  }

  def update(v : models.Volume.Id, i : models.Record.Id) = check(i, Permission.EDIT) { record => implicit request =>
    val (meas, formin) = editFormFill(record)
    val form = formin.bindFromRequest
    form.fold(
      form => BadRequest(views.html.record.edit(record, meas, form)),
      { case (category, data, (metric, datum)) =>
        record.change(category = category.flatMap(RecordCategory.get(_)))
        def update(metric : Metric.Id, datum : Option[String]) : Option[String] =
          Metric.get(metric).fold(Some("measure.unknown") : Option[String]) { m =>
            datum.fold {
              record.deleteMeasure(m)
              None : Option[String]
            } { value =>
              if (!record.setMeasure(m, value))
                Some("measure.bad")
              else
                None
            }
          }
        data.zipWithIndex.foldLeft {
          if (metric == 0)
            form
          else
            update(metric, datum).fold(form)(form.withError("add.datum", _))
        } { (form, measure) =>
          val ((metric, datum), i) = measure
          update(metric, datum).fold(form)(form.withError("measure.datum[" + i + "]", _))
        }.fold(
          form => BadRequest(views.html.record.edit(record, meas, form)),
          _ => Redirect(record.pageURL)
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

  def slotRemove(v : models.Volume.Id, s : models.Slot.Id, r : models.Record.Id) = Slot.check(s, Permission.EDIT) { slot => implicit request =>
    slot.removeRecord(r)
    Redirect(slot.pageURL)
  }

  def slotAdd(v : models.Volume.Id, s : models.Slot.Id) = Slot.check(s, Permission.EDIT) { slot => implicit request =>
    val form = selectForm.bindFromRequest
    form.fold(
      form => BadRequest(Slot.viewEdit(slot)(recordForm = form)),
      _.fold({
          val r = models.Record.create(slot.volume)
          r.addSlot(slot)
          Created(views.html.record.edit(r, Nil, editForm)) : Result
        })(models.Record.get(_).
          filter(r => r.permission >= Permission.DOWNLOAD && r.volumeId == slot.volumeId).
          fold(
            BadRequest(Slot.viewEdit(slot)(recordForm = form.withError("record", "record.bad"))) : Result){ r =>
              r.addSlot(slot)
              Redirect(slot.pageURL)
            }
        )
    )
  }

}
