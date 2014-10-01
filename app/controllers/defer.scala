package controllers

import scala.language.higherKinds
import scala.concurrent.Future
import play.api.mvc._
import play.api.libs.iteratee.{Iteratee,Enumerator}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

final case class EmptyRequest(request : RequestHeader) extends Request[AnyContentAsEmpty.type] {
  def body = AnyContentAsEmpty
  def id = request.id
  def tags = request.tags
  def headers = request.headers
  def queryString = request.queryString
  def path = request.path
  def uri = request.uri
  def method = request.method
  def version = request.version
  def remoteAddress = request.remoteAddress
  def secure = request.secure
}

/** An action that is created by pre-processing the RequestHeader alone first (with an empty body) using an ActionBuilder[R] to produce a R for input into the main action body.
  * Basically this allows existing ActionBuilders to be used in cases when body processing should be deferred, and all the necessary content for the builder is in the header already.
  */
sealed class DeferredAction[R[_]](builder : ActionBuilder[R], block : R[AnyContentAsEmpty.type] => Iteratee[Array[Byte], Result]) extends EssentialAction {
  protected class DeferralResult(val request : R[AnyContentAsEmpty.type], header : ResponseHeader = ResponseHeader(0)) extends Result(header, Enumerator()) {
    override def withHeaders(headers: (String, String)*) : Result =
      new DeferralResult(request, header.copy(headers = header.headers ++ headers))
  }

  def apply(request : RequestHeader) : Iteratee[Array[Byte], Result] = Iteratee.flatten {
    builder.invokeBlock[AnyContentAsEmpty.type](EmptyRequest(request), r => Future.successful(new DeferralResult(r)))
    .map {
      case dr : DeferralResult if dr.header.status == 0 =>
        block(dr.request).map(_.withHeaders(dr.header.headers.toSeq : _*))
      case r => Iteratee.ignore.map(_ => r)
    }
  }
}

final class DeferredSiteAction[R[_] <: SiteRequest[_]](builder : ActionBuilder[R], block : R[AnyContentAsEmpty.type] => Iteratee[Array[Byte], Result])
  extends DeferredAction(builder, block) {
  override def apply(request : RequestHeader) : Iteratee[Array[Byte], Result] = Iteratee.flatten {
    builder.invokeBlock[AnyContentAsEmpty.type](EmptyRequest(request), r => Future.successful(new DeferralResult(r)))
    .map {
      case dr : DeferralResult if dr.header.status == 0 =>
        scala.util.control.Exception.handling(classOf[SiteException]).by(e =>
          Iteratee.ignore[Array[Byte]].mapM(_ => e.asInstanceOf[SiteException].result(dr.request)))(
          block(dr.request).map(_.withHeaders(dr.header.headers.toSeq : _*)))
        .recoverM {
          case e : SiteException => e.result(dr.request)
        }
      case r => Iteratee.ignore[Array[Byte]].map(_ => r)
    }
  }
}

object DeferredAction {
  /** Use builder to pre-process the RequestHeader before passing the result onto block. */
  def apply[R[_]](builder : ActionBuilder[R])(block : R[AnyContentAsEmpty.type] => Iteratee[Array[Byte], Result]) : EssentialAction =
    new DeferredAction(builder, block)
  def site[R[_] <: SiteRequest[_]](builder : ActionBuilder[R])(block : R[AnyContentAsEmpty.type] => Iteratee[Array[Byte], Result]) : EssentialAction =
    new DeferredSiteAction(builder, block)
}
