package work.unformed.hardcore.http

import akka.http.scaladsl.marshalling.Marshaller
import akka.http.scaladsl.model._
import io.circe.Encoder
import work.unformed.hardcore.dsl._

object EventMarshaller {
  import CirceSupport._

  implicit def eventMarshaller[A : Encoder]: Marshaller[Event[A], HttpResponse] = Marshaller
    .withFixedContentType(ContentTypes.`application/json`){
      case Created(_, value) => HttpResponse(
        status = StatusCodes.Created,
        entity = printer.pretty(implicitly[Encoder[A]].apply(value))
      )
      case Result(_, value) => HttpResponse(
        status = StatusCodes.OK,
        entity = printer.pretty(implicitly[Encoder[A]].apply(value))
      )
      case Updated(_, _, value) => HttpResponse(
        status = StatusCodes.OK,
        entity = printer.pretty(implicitly[Encoder[A]].apply(value))
      )
      case Deleted(_, value) => HttpResponse(
        status = StatusCodes.NoContent
      )

      case NotFound(_) => HttpResponse(
        status = StatusCodes.NotFound
      )
      case AlreadyExists(_) => HttpResponse(
        status = StatusCodes.PreconditionFailed
      )

      case e: Error[A] => HttpResponse(
        status = StatusCodes.InternalServerError
      )
    }
}
