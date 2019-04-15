package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import io.circe.{Decoder, Encoder}
import dev.rudiments.hardcore.dsl._

class CrudRouter[A : Meta : Encoder : Decoder](prefix: String, handler: CommandHandler[A], idDirective: Directive1[ID[A]]) extends Router {
  import dev.rudiments.hardcore.http.CirceSupport._

  override val routes: Route = pathPrefix(prefix) {
    pathEndOrSingleSlash {
      get {
        complete(s"GET Query on $prefix")
      } ~ post {
        entity(as[A]) { draft =>
          responseWith(handler.handle(Create(draft)))
        }
      } ~ put {
        entity(as[List[A]]) { batch =>
          responseWith(handler.handle(CreateBatch(batch)))
        }
      } ~ delete {
        responseWith(handler.handle(DeleteAll()))
      }
    } ~ idDirective { id =>
      get {
        responseWith(handler.handle(Read(id)))
      } ~ put {
        entity(as[A]) { newValue =>
          responseWith(handler.handle(Update(newValue)))
        }
      } ~ delete {
        responseWith(handler.handle(Delete(id)))
      }
    }
  }

  def responseWith[A : Encoder](event: Event[A]): StandardRoute = event match {
    case Created(_, value) =>     complete(StatusCodes.Created, value)
    case Result(_, value) =>      complete(StatusCodes.OK, value)
    case Updated(_, _, value) =>  complete(StatusCodes.OK, value)
    case Deleted(_, _) =>         complete(StatusCodes.NoContent)

    case BatchCreated(_) =>       complete(StatusCodes.Created)
    case AllDeleted() =>          complete(StatusCodes.NoContent)

    case NotFound(_) => complete(StatusCodes.NotFound)
    case AlreadyExists(_) => complete(StatusCodes.Conflict)

    case e: Error[A] => complete(StatusCodes.InternalServerError)
  }
}
