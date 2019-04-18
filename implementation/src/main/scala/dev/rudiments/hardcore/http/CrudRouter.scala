package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import io.circe.{Decoder, Encoder}
import dev.rudiments.hardcore.dsl._

//TODO remove asInstanceOf[DataEvent|BatchEvent]
class CrudRouter[A : Meta : Encoder : Decoder](
  prefix: String,
  handler: PartialFunction[Command, Event],
  idDirective: Directive1[ID[A]]
) extends Router {
  import dev.rudiments.hardcore.http.CirceSupport._
  import dev.rudiments.hardcore.dsl.ID._

  override val routes: Route = pathPrefix(prefix) {
    pathEndOrSingleSlash {
      get {
        complete(s"GET Query on $prefix")
      } ~ post {
        entity(as[A]) { value =>
          responseWith(handler(Create(value.identify, value)).asInstanceOf[DataEvent[ID[A], A]])
        }
      } ~ put {
        entity(as[List[A]]) { batch =>
          responseBatchWith(handler(CreateAll(batch.groupBy(_.identify).mapValues(_.head))).asInstanceOf[BatchEvent[ID[A], A]])
        }
      } ~ delete {
        responseBatchWith(handler(DeleteAll()).asInstanceOf[BatchEvent[ID[A], A]])
      }
    } ~ idDirective { id =>
      get {
        responseWith(handler(Read(id)).asInstanceOf[DataEvent[ID[A], A]])
      } ~ put {
        entity(as[A]) { newValue =>
          responseWith(handler(Update(newValue.identify, newValue)).asInstanceOf[DataEvent[ID[A], A]])
        }
      } ~ delete {
        responseWith(handler(Delete[ID[A], A](id)).asInstanceOf[DataEvent[ID[A], A]])
      }
    }
  }

  def responseWith(event: DataEvent[ID[A], A]): StandardRoute = event match {
    case Created(_, value) =>     complete(StatusCodes.Created, value)
    case Result(_, value) =>      complete(StatusCodes.OK, value)
    case Updated(_, _, value) =>  complete(StatusCodes.OK, value)
    case Deleted(_, _) =>         complete(StatusCodes.NoContent)

    case NotFound(_) =>      complete(StatusCodes.NotFound)
    case AlreadyExists(_) => complete(StatusCodes.Conflict)

    case _: Error => complete(StatusCodes.InternalServerError)
  }

  def responseBatchWith(event: BatchEvent[ID[A], A]): StandardRoute = event match {
    case AllCreated(_) => complete(StatusCodes.Created)
    case AllDeleted() =>  complete(StatusCodes.NoContent)

    case _: Error => complete(StatusCodes.InternalServerError)
  }
}
