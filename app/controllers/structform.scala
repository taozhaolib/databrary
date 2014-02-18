package controllers

import scala.collection.mutable
import scala.concurrent.{ExecutionContext,Future}
import play.api.data._
import play.api.data.validation._
import play.api.mvc.Call

/** This is an alternative to play.api.data.Form that provides more structure and safety.
  * The disadvantage of this class is mutability and less efficiency. */
abstract class StructForm {
  self =>

  /** A field in this form, which should only be used to declare vals. */
  protected final case class Field[T](map : Mapping[T]) {
    private[this] var _name : String = _
    def name : String = _name
    private[StructForm] def name_=(name : String) {
      _name = name
    }
    /** The value of this field, which will be filled in by binding the form. */
    var value : T = _
    def init(v : T) : Field[T] = {
      value = v
      this
    }

    private[StructForm] lazy val mapping : Mapping[T] = map.withPrefix(name)
    private[StructForm] def bind(data : Map[String,String]) : Option[Seq[FormError]] =
      mapping.bind(data).fold(Some(_), v => { value = v ; None })
    private[StructForm] def unbind = mapping.unbind(value)
  }

  private[this] def getValFields = {
    val cls = getClass
    cls.getDeclaredFields.toIterator
      .filter(f => classOf[Field[_]].isAssignableFrom(f.getType))
      .map { f =>
	val name = f.getName
	val field = cls.getDeclaredMethod(name).invoke(self).asInstanceOf[Field[_]]
	field.name = name
	field
      }.toSeq
  }
  private[this] lazy val fields = getValFields

  private[this] var _data : Map[String, String] = Map.empty[String, String]
  private[this] val _errors : mutable.ListBuffer[FormError] = mutable.ListBuffer.empty[FormError]
  private[this] val _constraints : mutable.ListBuffer[Constraint[self.type]] = mutable.ListBuffer.empty[Constraint[self.type]]
  final def hasErrors = _errors.nonEmpty

  protected object _mapping extends Mapping[self.type] {
    val key : String = ""
    val constraints : Seq[Constraint[self.type]] = _constraints
    def bind(data : Map[String,String]) : Either[Seq[FormError], self.type] = {
      val l = fields.flatMap(_.bind(data))
      if (l.isEmpty)
        Right(self)
      else
        Left(l.flatten)
    }
    def unbind(value : self.type) : (Map[String, String], Seq[FormError]) = {
      val (m, e) = fields.map(_.unbind).unzip
      (m.fold(Map.empty[String, String])(_ ++ _), e.flatten[FormError])
    }
    val mappings : Seq[Mapping[_]] =
      this +: fields.flatMap(_.mapping.mappings)
    def withPrefix(prefix : String) : Mapping[self.type] =
      throw new UnsupportedOperationException("StructForm.mapping.withPrefix")
    def verifying(c : Constraint[self.type]*) : Mapping[self.type] = {
      _constraints ++= c
      this
    }
  }

  protected class form(value : Option[self.type] = None) extends Form[self.type](_mapping, _data, _errors, value) {
    override def bind(data : Map[String, String]) : Form[self.type] = {
      _data = data
      mapping.bind(data).fold(
	errors => { _errors ++= errors ; new form(None) },
	value => new form(Some(value))
      )
    }
    override def fill(value : self.type) : Form[self.type] = {
      val (data, _) = mapping.unbind(value)
      _data = data
      new form(Some(value))
    }
    override def fillAndValidate(value : self.type) : Form[self.type] = {
      val (data, errors) = mapping.unbind(value)
      _data = data
      _errors.clear
      _errors ++= errors
      new form(Some(value))
    }
    override def withError(error : FormError) : Form[self.type] = {
      _errors += error
      new form(None)
    }
    override def discardingErrors : Form[self.type] = {
      _errors.clear
      this
    }
  }
  def apply() = new form()
  protected def _fill() {
    _data = _mapping.unbind(self)._1
  }
}

abstract class FormView[F <: FormView[F]](val _action : Call) extends StructForm {
  def _exception : FormException
  final def orThrow() {
    if (hasErrors)
      throw _exception
  }
}

class HtmlForm[F <: HtmlForm[F]](action : Call, errorView : F => play.api.templates.HtmlFormat.Appendable) extends FormView[F](action) {
  self : F =>
  final def _exception = new FormException(new form()) {
    def resultHtml(implicit site : SiteRequest[_]) = macros.Async(BadRequest(errorView(self)))
  }
}

class AHtmlForm[F <: HtmlForm[F]](action : Call, errorView : F => Future[play.api.templates.HtmlFormat.Appendable])(implicit context : ExecutionContext) extends FormView[F](action) {
  self : F =>
  final def _exception = new FormException(new form()) {
    def resultHtml(implicit site : SiteRequest[_]) = errorView(self).map(BadRequest(_))
  }
}

class ApiForm[F <: ApiForm[F]](action : Call) extends FormView[F](action) {
  final def _exception = new ApiFormException(new form())
}
