package controllers

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api._
import          templates.Html
import          Play.current
import          mvc._
import          data._
import               Forms._
import play.api.libs.json.Json
import site._
import models._

package object Record extends ObjectController[Record] {
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

  private val jsonCategories = {
    Html(Json.stringify(Json.toJson(RecordCategory.getAll.map {
      case c =>
        Json.toJson(Map(
          "id" -> Json.toJson(c.id.toString),
          "name" -> Json.toJson(c.name),
          "template" -> Json.toJson(c.template.map {
            case m =>
              Json.toJson(m.id.toString)
          })
        ))
    }
    )))
  }

  private val jsonMetrics = {
    Html(Json.stringify(Json.toJson(Metric.getAll.map {
      case m =>
        Json.toJson(Map(
          "id" -> Json.toJson(m.id.toString),
          "name" -> Json.toJson(m.name),
          "dataType" -> Json.toJson(m.dataType.toString),
          "classification" -> Json.toJson(m.classification.toString),
          "values" -> Json.toJson(m.values)
        ))
      }
    )))
  }

  type SelectForm = Form[Option[models.Record.Id]]
  protected[controllers] val selectForm = Form(
    "record" -> optional(of[models.Record.Id])
  )

  private[controllers] def selectList(target : Slot)(implicit request : SiteRequest[_]) : Future[Seq[(String, String)]] =
    /* ideally we'd remove already used records here */
    target.volume.allRecords().map(_ map { r : Record =>
      (r.id.toString, r.category.fold("")(_.name + ':') + r.ident)
    })

  object html {
    private def action(v : models.Volume.Id, i : models.Record.Id, p : Permission.Value = Permission.VIEW) =
      RequestObject.check(v, models.Record.get(i)(_), p)

    private def Action(v : models.Volume.Id, i : models.Record.Id, p : Permission.Value = Permission.VIEW) =
      SiteAction ~> action(v, i, p)

    def view(v : models.Volume.Id, i : models.Record.Id) = Action(v, i).async { implicit request =>
      for {
        assets <- request.obj.assets
      } yield (Ok(views.html.record.view(assets)))
    }

    private def editFormFill(implicit request : Request[_]) : (Seq[Metric[_]], EditForm) = {
      val r = request.obj
      val m = r.measures.list
      val mm = m.map(_.metric)
      val t = r.category.fold[Seq[Metric[_]]](Nil)(_.template).diff(mm)
      (mm ++ t, editForm.fill(
        (
          r.categoryId,
          m.map(m => (m.metricId, Some(m.datum))) ++ t.map(_.id -> None)
        )
      ))
    }

    def edit(v : models.Volume.Id, i : models.Record.Id) = Action(v, i, Permission.EDIT) { implicit request =>
      val (m, f) = editFormFill
      Ok(views.html.record.edit(request.obj, m, f, jsonCategories, jsonMetrics))
    }

    def update(v : models.Volume.Id, i : models.Record.Id) = Action(v, i, Permission.EDIT).async { implicit request =>
      val (meas, formin) = editFormFill
      val form = formin.bindFromRequest
      form.fold(
        form => ABadRequest(views.html.record.edit(request.obj, meas, form, jsonCategories, jsonMetrics)),
        { case (category, data) =>
          request.obj.change(category = category.flatMap(RecordCategory.get(_))).flatMap { _ =>
          val filled = scala.collection.mutable.Set.empty[Int] // temporary hack to prevent data corruption with duplicate metrics
          def update(metric : Metric.Id, datum : Option[String]) : Future[Option[String]] =
            Metric.get(metric).fold[Future[Option[String]]](
              macros.Async(Some("measure.unknown"))
            ) { m =>
              datum.fold[Future[Option[String]]] {
                if (!filled.contains(metric.unId))
                  request.obj.removeMeasure(m).map(_ => None)
                else
                  macros.Async(None)
              } { value =>
                filled.add(metric.unId)
                request.obj.setMeasure(m, value).map {
                  case false => Some("measure.bad")
                  case true => None
                }
              }
            }
          macros.Async.map(data, (update _).tupled).map {
          _.zipWithIndex.foldLeft(form) { (form, error) => error match {
            case (None, _) => form
            case (Some(error), i) => form.withError("measure.datum[" + i + "]", error)
          } }.fold(
            form => BadRequest(views.html.record.edit(request.obj, meas, form, jsonCategories, jsonMetrics)),
            _ => Redirect(request.obj.pageURL)
          )
          } }
        }
      )
    }

    def slotRemove(v : models.Volume.Id, s : models.Slot.Id, r : models.Record.Id, editRedirect : Boolean = false) = SlotHtml.ActionId(v, s, Permission.EDIT).async { implicit request =>
      request.obj.removeRecord(r).map { _ =>
        if (editRedirect)
          Redirect(controllers.routes.SlotHtml.edit(v, s))
        else
          Redirect(request.obj.pageURL)
      }
    }

    def slotAdd(v : models.Volume.Id, s : models.Slot.Id, catID : models.RecordCategory.Id, editRedirect : Boolean = false) = SlotHtml.ActionId(v, s, Permission.EDIT).async { implicit request =>
      val form = selectForm.bindFromRequest
      form.fold(
        form => SlotHtml.viewEdit(SlotHtml.BadRequest, request.obj)(recordForm = form),
        _.fold {
          val cat = RecordCategory.get(catID)
          for {
            r <- models.Record.create(request.obj.volume, cat)
            _ <- r.addSlot(request.obj)
          } yield (Created(views.html.record.edit(r, cat.fold[Seq[Metric[_]]](Nil)(_.template), editForm.fill((cat.map(_.id), Seq())), jsonCategories, jsonMetrics)))
        } (models.Record.get(_).flatMap(_
          .filter(r => r.checkPermission(Permission.DOWNLOAD) && r.volumeId === v)
          .fold(
            SlotHtml.viewEdit(SlotHtml.BadRequest, request.obj)(recordForm = form.withError("record", "record.bad"))
          ) { r => r.addSlot(request.obj).map { _ =>
            if (editRedirect)
              Redirect(controllers.routes.SlotHtml.edit(v, s))
            else
              Redirect(request.obj.pageURL)
          } }
        ))
      )
    }

    def add(v : models.Volume.Id, catID : models.RecordCategory.Id) = VolumeController.Action(v, Permission.EDIT).async { implicit request =>
      val cat = RecordCategory.get(catID)
      models.Record.create(request.obj.volume, cat).map { r =>
        Created(views.html.record.edit(r, cat.fold[Seq[Metric[_]]](Nil)(_.template), editForm.fill((cat.map(_.id), Seq())), jsonCategories, jsonMetrics))
      }
    }
  }

  object api {
    private def action(i : models.Record.Id, p : Permission.Value = Permission.VIEW) =
      RequestObject.check(models.Record.get(i)(_), p)

    private def Action(i : models.Record.Id, p : Permission.Value = Permission.VIEW) =
      SiteAction ~> action(i, p)

    def get(i : models.Record.Id) = Action(i).async { implicit request =>
      request.obj.json(request.apiOptions).map(Ok(_))
    }

    private implicit val categoryFormatter : format.Formatter[RecordCategory] = new format.Formatter[RecordCategory] {
      def bind(key : String, data : Map[String, String]) =
        data.get(key).flatMap(RecordCategory.getName(_))
          .toRight(Seq(FormError(key, "error.invalid", Nil)))
      def unbind(key : String, value : RecordCategory) =
        Map(key -> value.name)
    }

    private val queryForm = Form(single(
      "category" -> optional(of[RecordCategory])
    ))

    def query(volume : models.Volume.Id) = VolumeController.Action(volume).async { implicit request =>
      queryForm.bindFromRequest.fold(
        form => ABadRequest(Json.toJson(form.errors)),
        category =>
          request.obj.allRecords(category).map(l =>
            Ok(JsonRecord.map[Record](_.json)(l)))
      )
    }
  }
}
