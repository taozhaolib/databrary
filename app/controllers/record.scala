package controllers

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api._
import          templates.Html
import          Play.current
import          mvc._
import          data._
import play.api.i18n.Messages
import play.api.libs.json
import macros.async._
import site._
import dbrary._
import models._

private[controllers] abstract sealed class RecordController extends ObjectController[Record] {
  protected def action(i : models.Record.Id, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(models.Record.get(i)(_), p)

  protected def Action(i : models.Record.Id, p : Permission.Value = Permission.VIEW) =
    SiteAction andThen action(i, p)

  protected final val categoryMapping : Mapping[RecordCategory] =
    Forms.of[RecordCategory.Id]
      .transform[Option[RecordCategory]](RecordCategory.get(_), _.get.id)
      .verifying(Messages("error.invalid"), _.isDefined)
      .transform(_.get, Some(_))
  protected final val metricMapping : Mapping[Metric[_]] =
    Forms.of[Metric.Id]
      .transform[Option[Metric[_]]](Metric.get(_), _.get.id)
      .verifying(Messages("error.invalid"), _.isDefined)
      .transform(_.get, Some(_))

  private[this] def editResult(record : Record)(implicit request : SiteRequest[_]) : Result =
    if (request.isApi) result(record)
    else Redirect(routes.RecordHtml.edit(record.id))

  def update(i : models.Record.Id) =
    Action(i, Permission.EDIT).async { implicit request =>
      val form = new RecordController.EditForm()._bind
      for {
        _ <- request.obj.change(category = form.category.get)
      } yield (editResult(request.obj))
    }

  def measureUpdate(recordId : Record.Id, metricId : Metric.Id) =
    Action(recordId, Permission.EDIT).async { implicit request =>
      val metric = Metric.get(metricId).getOrElse(throw NotFoundException)
      val form = new RecordController.MeasureForm(metric)._bind
      form.datum.get.fold(
        request.obj.removeMeasure(metric))(d =>
        request.obj.setMeasure(new Measure(metric, d)).map {
          case false => form.datum.withError("error.invalid")._throw
          case true => true
        })
      .map(_ => editResult(request.obj))
    }

  def add(containerId : Container.Id, segment : Segment) =
    SlotHtml.Action(containerId, segment, Permission.EDIT).async { implicit request =>
      val form = new RecordController.SelectForm()._bind
      form.record.get.fold {
        for {
          r <- models.Record.create(request.obj.volume, form.category.get)
          _ <- r.addSlot(request.obj)
        } yield (editResult(r))
      } { r =>
        for {
          mr <- models.Record.get(r)
          r = mr.filter(r => r.checkPermission(Permission.SHARED) && r.volumeId === request.obj.volumeId)
            .getOrElse(form.record.withError("record.bad")._throw)
          _ <- r.addSlot(request.obj)
        } yield (if (request.isApi) result(r) else SlotController.result(request.obj))
      }
    }

  def remove(containerId : Container.Id, segment : Segment, recordId : Record.Id) = Action(recordId, Permission.EDIT).async { implicit request =>
    for {
      so <- Slot.get(containerId, segment)
      s = so.getOrElse(throw NotFoundException)
      _ <- request.obj.removeSlot(s)
    } yield (SlotController.result(s))
  }
}

object RecordController extends RecordController {
  final class EditForm(implicit request : Request[_])
    extends HtmlForm[EditForm](
      routes.RecordHtml.update(request.obj.id),
      f => RecordHtml.viewEdit(editForm = Some(f))) {
    val category = Field(OptionMapping(Forms.optional(categoryMapping))).fill(Some(request.obj.category))
  }

  final class SelectForm(implicit request : SlotHtml.Request[_])
    extends AHtmlForm[SelectForm](
      routes.RecordHtml.add(request.obj.containerId, request.obj.segment),
      f => SlotHtml.viewEdit(recordForm = Some(f))) {
    val record = Field(Forms.optional(Forms.of[models.Record.Id]))
    val category = Field(Forms.optional(categoryMapping))
  }

  final class MeasureForm(val metric : Metric[_])(implicit request : Request[_])
    extends HtmlForm[MeasureForm](
      routes.RecordHtml.measureUpdate(request.obj.id, metric.id),
      f => RecordHtml.viewEdit(measureForm = Some(f))) {
    val datum = Field(Mappings.maybeText).fill(None)
    private[controllers] def _fill(d : String) : this.type = {
      datum.fill(Some(d))
      this
    }
  }
}

object RecordHtml extends RecordController with HtmlController {
  import RecordController._

  def view(i : models.Record.Id) = Action(i).async { implicit request =>
    for {
      slots <- request.obj.slots
      _ <- slots.foreachAsync(_.records)
    } yield (Ok(views.html.record.view(slots)))
  }

  def viewEdit(editForm : Option[EditForm] = None, measureForm : Option[MeasureForm] = None, addForm : Option[MetricForm] = None)(implicit request : Request[_]) = {
    val r = request.obj
    val m = r.measures.list.toSeq
    val t = r.category.fold[Seq[Metric[_]]](Nil)(_.template) diff m.map(_.metric)
    val mf : Seq[MeasureForm] = m.map(m => new MeasureForm(m.metric)._fill(m.datum)) ++
      t.map(new MeasureForm(_))
    views.html.record.edit(
      editForm.getOrElse(new EditForm),
      mf.map(m => measureForm.filter(_.metric === m.metric).getOrElse(m)) ++
        measureForm.filterNot(m => mf.exists(_.metric === m.metric)),
      addForm.getOrElse(new MetricForm))
  }

  def edit(i : models.Record.Id) =
    Action(i, Permission.EDIT) { implicit request =>
      Ok(viewEdit())
    }

  final class MetricForm(implicit request : Request[_])
    extends HtmlForm[MetricForm](
      routes.RecordHtml.measureAdd(request.obj.id),
      f => RecordHtml.viewEdit(addForm = Some(f))) {
    val metric = Field(metricMapping)
    private[RecordHtml] def measureForm = new MeasureForm(metric.get)
  }

  def measureAdd(recordId : Record.Id) =
    Action(recordId, Permission.EDIT) { implicit request =>
      val form = new MetricForm()._bind
      Ok(viewEdit(measureForm = Some(form.measureForm)))
    }

  final class RemoveForm(record : Record, slot : Slot)
    extends StructForm(routes.RecordHtml.remove(slot.containerId, slot.segment, record.id))
}

object RecordApi extends RecordController with ApiController {
  def get(i : models.Record.Id) = Action(i).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  private val queryForm = Form(Forms.single(
    "category" -> Forms.optional(categoryMapping)
  ))

  def query(volume : models.Volume.Id) = VolumeController.Action(volume).async { implicit request =>
    queryForm.bindFromRequest.fold(
      new ApiFormException(_).result,
      category =>
        request.obj.records(category).map(l =>
          Ok(JsonArray.map[Record,JsonRecord](_.json)(l)))
    )
  }

  def create(v : models.Volume.Id, catId : Option[RecordCategory.Id]) =
    VolumeController.Action(v, Permission.EDIT).async { implicit request =>
      val cat = catId.map(RecordCategory.get(_).getOrElse(throw NotFoundException))
      models.Record.create(request.obj, cat).map(result(_))
    }
}
