package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import io.circe.{Decoder, Encoder}
import dev.rudiments.hardcore.dsl._

class CrudRouter[A : Meta : Encoder : Decoder](prefix: String, handler: DataCommandHandler[ID[A], A], idDirective: Directive1[ID[A]]) extends Router {
  import dev.rudiments.hardcore.http.CirceSupport._
  import dev.rudiments.hardcore.dsl.ID._

  override val routes: Route = pathPrefix(prefix) {
    pathEndOrSingleSlash {
      get {
        complete(s"GET Query on $prefix")
      } ~ post {
        entity(as[A]) { value =>
          responseWith(handler.handle(Create(value.identify, value)))
        }
      } ~ put {
        entity(as[List[A]]) { batch =>
          responseWith(handler.handle(CreateAll(batch.groupBy(_.identify).mapValues(_.head))))
        }
      } ~ delete {
        responseWith(handler.handle(DeleteAll()))
      }
    } ~ idDirective { id =>
      get {
        responseWith(handler.handle(Read(id)))
      } ~ put {
        entity(as[A]) { newValue =>
          responseWith(handler.handle(Update(newValue.identify, newValue)))
        }
      } ~ delete {
        responseWith(handler.handle(Delete[ID[A], A](id)))
      }
    }
  }

  def responseWith[A : Encoder](event: DataEvent[ID[A], A]): StandardRoute = event match {
    case Created(_, value) =>     complete(StatusCodes.Created, value)
    case Result(_, value) =>      complete(StatusCodes.OK, value)
    case Updated(_, _, value) =>  complete(StatusCodes.OK, value)
    case Deleted(_, _) =>         complete(StatusCodes.NoContent)

    case AllCreated(_) =>         complete(StatusCodes.Created)
    case AllDeleted() =>          complete(StatusCodes.NoContent)

    case NotFound(_) =>      complete(StatusCodes.NotFound)
    case AlreadyExists(_) => complete(StatusCodes.Conflict)

    case _: Error => complete(StatusCodes.InternalServerError)
  }
}
