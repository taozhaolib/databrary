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

  def view(i : models.Record.Id) = check(i) { record => implicit request =>
    Ok(views.html.record.view(record))
  }

  private type MeasureMapping = (Metric.Id, Option[String])
  private val measureMapping = tuple(
    "metric" -> of[Metric.Id],
    "datum" -> optional(nonEmptyText)
  )
  type RecordForm = Form[(Option[RecordCategory.Id], Seq[MeasureMapping], MeasureMapping)]
  private val recordForm = Form(tuple(
    "category" -> optional(of[RecordCategory.Id]),
    "measure" -> seq(measureMapping),
    "add" -> measureMapping
  ))

  private def recordFormFill(r : Record)(implicit site : Site) : (Seq[MetricBase], RecordForm) = {
    val m = r.measures
    val mm = m.map(_.metric)
    val t = r.category.fold(Nil : Seq[MetricBase])(_.template).diff(mm)
    (mm ++ t, recordForm.fill((
      r.categoryId,
      m.map(m => (m.metricId, Some(m.datum.toString))) ++ t.map(m => (m.id, None)),
      (Metric.asId(0), None)
    )))
  }

  def edit(i : models.Record.Id) = check(i, Permission.EDIT) { record => implicit request =>
    val (m, f) = recordFormFill(record)
    Ok(views.html.record.edit(record, m, f))
  }

  def update(i : models.Record.Id) = check(i, Permission.EDIT) { record => implicit request =>
    val (meas, formin) = recordFormFill(record)
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

}
