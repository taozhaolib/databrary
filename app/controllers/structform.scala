package controllers

import scala.collection.mutable
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.data._
import play.api.data.validation._
import play.api.mvc._
import play.api.templates.HtmlFormat
import play.api.libs.Files
import macros._

/** This is an alternative to play.api.data.Form that provides more structure and safety.
  * The disadvantage of this class is mutability and less efficiency. */
abstract class StructForm(val _action : Call) {
  self =>

  protected sealed abstract class Member[T] {
    private[this] var _name : String = _
    final def name : String = _name
    private[StructForm] final def name_=(name : String) {
      _name = name
    }
    /** The value of this field, which will be filled in by binding the form. */
    protected var value : T = _
    def get : T = value

    final def apply() = self()(name)
    final def withKeyError(key : String, message : String, args : Any*) : self.type =
      self.withError(FormError(name + Maybe.bracket(".", key), message, args))
    final def withError(message : String, args : Any*) : self.type =
      withKeyError("", message, args : _*)
  }

  /** A field in this form, which should only be used to declare vals. */
  protected final case class Field[T](map : Mapping[T]) extends Member[T] {
    def fill(v : T) : Field[T] = {
      value = v
      if (name != null)
        _data ++= unbind
      this
    }
    private[this] lazy val mapping : Mapping[T] = map.withPrefix(name.ensuring(_ != null))
    private[StructForm] def mappings : Seq[Mapping[_]] = mapping.mappings
    private[StructForm] def bind(data : Map[String,String]) : Option[Seq[FormError]] =
      mapping.bind(data).fold(Some(_), v => { value = v ; None })
    private[StructForm] def unbind : Map[String, String] =
      if (value == null) Map.empty[String,String] else mapping.unbind(value)
    private[StructForm] def unbindAndValidate : (Map[String, String], Seq[FormError]) =
      if (value == null)
        (Map.empty[String,String], Seq(FormError(name, "error.missing")))
      else
        mapping.unbindAndValidate(value)
  }

  protected def Const[T](x : T) : Field[T] =
    Field[T](Forms.ignored(x)).fill(x)

  final private type FileData = MultipartFormData[Files.TemporaryFile]
  final type FilePart = MultipartFormData.FilePart[Files.TemporaryFile]
  protected sealed abstract class FileMember[T] extends Member[T] {
    private[this] var constraints : Seq[Constraint[FilePart]] = Nil
    final def verifying(c : Constraint[FilePart]*) : this.type = {
      constraints ++= c
      this
    }
    final def verifying(constraint : FilePart => Boolean) : this.type =
      verifying("error.unknown", constraint)
    final def verifying(error : => String, constraint : FilePart => Boolean) : this.type = {
      verifying(Constraint { t : FilePart =>
        if (constraint(t)) Valid else Invalid(Seq(ValidationError(error)))
      })
    }
    protected final def applyConstraints(f : FilePart) : Seq[FormError] =
      constraints.map(_(f)).collect {
        case Invalid(errors) => errors.map(e => FormError(name, e.message, e.args))
      }.flatten
    def bind(body : FileData) : Seq[FormError]
  }

  protected final case class File() extends FileMember[FilePart] {
    def bind(body : FileData) =
      body.file(name)
      .fold(Seq(FormError(name, "error.required"))) { f =>
        value = f
        applyConstraints(f)
      }
  }

  protected final case class OptionalFile() extends FileMember[Option[FilePart]] {
    def bind(data : FileData) : Seq[FormError] = {
      value = data.file(name)
      (value : Iterable[FilePart]).flatMap(applyConstraints(_)).toSeq
    }
  }

  protected def _csrf(implicit request : RequestHeader) : Option[String] =
    Some(play.api.libs.Crypto.sign(request.session.toString + _action.toString))
  val csrf = Field(Mappings.raw)
  final def csrfField(implicit request : RequestHeader) =
    csrf().copy(value = _csrf)
  private[this] def csrfCheck(implicit request : Request[AnyContent]) {
    if (request.headers.get("X-Requested-With").isDefined ||
      !(request.method.equals("GET") || (request.method.equals("POST") &&
        request.contentType.exists(t =>
            t.equals("application/x-www-form-urlencoded") || t.equals("text/plain") || t.equals("multipart/form-data")))) ||
      csrf.get.equals(_csrf))
      return
    csrf.withError("csrf.failed")
  }

  private[this] def getValMembers : Iterator[Member[_]] =
    getClass.getMethods.toIterator
      .filter(f => f.getModifiers == java.lang.reflect.Modifier.PUBLIC && f.getName()(0).isLower && f.getParameterTypes.isEmpty && f.getTypeParameters.isEmpty && classOf[Member[_]].isAssignableFrom(f.getReturnType))
      .map { f =>
        val field = f.invoke(self).asInstanceOf[Member[_]]
        field.name = f.getName
        field
      }
  private[this] lazy val (_fields, _files) =
    partition(getValMembers.toSeq,
      PartialFunction[Member[_], Either[Field[_], FileMember[_]]] {
        case f : Field[_] => Left(f)
        case f : FileMember[_] => Right(f)
      }
    )

  protected object _mapping extends Mapping[self.type] {
    val key : String = ""
    val constraints : Seq[Constraint[self.type]] = _constraints
    def bind(data : Map[String,String]) : Either[Seq[FormError], self.type] = {
      val l = _fields.flatMap(_.bind(data))
      if (l.isEmpty)
        Right(self)
      else
        Left(l.flatten)
    }
    def unbind(value : self.type) : Map[String, String] =
      _fields.foldLeft(Map.empty[String, String])(_ ++ _.unbind)
    def unbindAndValidate(value : self.type) : (Map[String, String], Seq[FormError]) = {
      val (m, e) = _fields.map(_.unbindAndValidate).unzip
      (m.fold(Map.empty[String, String])(_ ++ _), e.flatten[FormError])
    }
    val mappings : Seq[Mapping[_]] =
      this +: _fields.flatMap(_.mappings)
    def withPrefix(prefix : String) : Mapping[self.type] =
      throw new UnsupportedOperationException("StructForm.mapping.withPrefix")
    def verifying(c : Constraint[self.type]*) : Mapping[self.type] = {
      _constraints ++= c
      this
    }
  }

  protected class form(value : Option[self.type] = if (hasErrors) None else Some(self)) extends Form[self.type](_mapping, _data.toMap, _errors, value) {
    override def bind(data : Map[String, String]) : Form[self.type] = {
      _data.clear
      _data ++= data
      mapping.bind(data).fold(
        errors => { _errors ++= errors ; new form(None) },
        value => new form(Some(value))
      )
    }
    override def fill(value : self.type) : Form[self.type] = {
      _data.clear
      _data ++= mapping.unbind(value)
      new form(Some(value))
    }
    override def fillAndValidate(value : self.type) : Form[self.type] = {
      _data.clear
      _errors.clear
      val (data, errors) = mapping.unbindAndValidate(value)
      _data ++= data
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

  private[this] lazy val _data : mutable.Map[String, String] = mutable.Map(_mapping.unbind(self).toSeq : _*)
  private[this] val _errors : mutable.ListBuffer[FormError] = mutable.ListBuffer.empty[FormError]
  private[this] val _constraints : mutable.ListBuffer[Constraint[self.type]] = mutable.ListBuffer.empty[Constraint[self.type]]
  final def hasErrors = _errors.nonEmpty

  /** Like this().globalErrors but also includes errors attached to missing fields, as this is a common bug. */
  def globalErrors : Seq[FormError] =
    _errors.filter(e => e.key.isEmpty || !_data.contains(e.key))
  def withError(error : FormError) : self.type = {
    _errors += error
    self
  }
  def withGlobalError(message : String, args : Any*) : self.type =
    withError(FormError("", message, args))

  private[this] def bindFiles(implicit request : Request[AnyContent]) : self.type = {
    val d = request.body.asMultipartFormData
      .getOrElse(MultipartFormData[Files.TemporaryFile](Map.empty, Nil, Nil, Nil))
    _errors ++= _files.flatMap(_.bind(d))
    self
  }
  def _bind(implicit request : Request[AnyContent]) : self.type = {
    apply().bindFromRequest
    bindFiles
    csrfCheck
    self
  }
  def _enctype : String =
    if (_files.nonEmpty) "multipart/form-data"
    else "application/x-www-form-urlencoded"
}

trait NoCsrfForm extends StructForm {
  override protected def _csrf(implicit request : RequestHeader) : Option[String] = None
}

abstract class FormView(action : Call) extends StructForm(action) {
  self =>
  def _exception(status : Results.Status = Results.BadRequest) : SiteException
  final def _throw = throw _exception()
  final def orThrow() : self.type = {
    if (hasErrors)
      _throw
    self
  }
  override def _bind(implicit request : Request[AnyContent]) : self.type = {
    super._bind.orThrow
  }
}

abstract class HtmlFormView(action : Call) extends FormView(action) {
  def _view : Future[HtmlFormat.Appendable]
  final def Ok : Future[Result] = _view.map(Results.Ok(_))
  final def Bad(implicit request : SiteRequest[_]) : Future[Result] = _exception().result
  final def _exception(status : Results.Status) =
    new FormException(new form(), status) {
      def resultHtml(implicit request : SiteRequest[_]) = _view.map(status(_))
    }
}

class HtmlForm[+F <: HtmlForm[F]](action : Call, view : F => HtmlFormat.Appendable) extends HtmlFormView(action) {
  this : F =>
  final def _view = async(view(this))
}
class AHtmlForm[+F <: AHtmlForm[F]](action : Call, view : F => Future[HtmlFormat.Appendable]) extends HtmlFormView(action) {
  this : F =>
  final def _view = view(this)
}

class ApiForm(action : Call) extends FormView(action) {
  final def _exception(status : Results.Status) =
    new ApiFormException(new form(), status)
}
