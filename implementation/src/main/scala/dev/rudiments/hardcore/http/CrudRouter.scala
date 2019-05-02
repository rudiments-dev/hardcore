package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import io.circe.{Decoder, Encoder}
import dev.rudiments.hardcore.dsl._


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
          responseWith(handler(Create(value.identify, value)))
        }
      } ~ put {
        entity(as[List[A]]) { batch =>
          responseWith(handler(CreateAll(batch.groupBy(_.identify).mapValues(_.head))))
        }
      } ~ delete {
        responseWith(handler(DeleteAll()))
      }
    } ~ idDirective { id =>
      get {
        responseWith(handler(Read(id)))
      } ~ put {
        entity(as[A]) { newValue =>
          responseWith(handler(Update(newValue.identify, newValue)))
        }
      } ~ delete {
        responseWith(handler(Delete[ID[A], A](id)))
      }
    }
  }

  def responseWith(event: Event): StandardRoute = event match {
    case c: Created[_, A] =>  complete(StatusCodes.Created, c.value)
    case c: Result[_, A] =>   complete(StatusCodes.OK, c.value)
    case c: Updated[_, A] =>  complete(StatusCodes.OK, c.newValue)
    case _: Deleted[_, A] =>  complete(StatusCodes.NoContent)

    case _: AllCreated[_, A] => complete(StatusCodes.Created)
    case _: AllDeleted[_, A] => complete(StatusCodes.NoContent)

    case _: NotFound[_, A] =>      complete(StatusCodes.NotFound)
    case _: AlreadyExists[_, A] => complete(StatusCodes.Conflict)

    case _: Error => complete(StatusCodes.InternalServerError)
  }
}
