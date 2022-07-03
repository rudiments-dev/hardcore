package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore.Memory.MemoryOps
import dev.rudiments.hardcore.Predicate.All
import dev.rudiments.hardcore._

class ScalaRouter(implicit val mount: Memory) extends CirceSupport {
  val routes: Route = {
    path(Segment) { str =>
      get {
        responseWith(ID(str).?)
      } ~ delete {
        responseWith(ID(str) << Delete)
      }/* ~ entity(as[Data]) { data =>
        post {
          responseWith(mount << Create(id, data))
        } ~ put {
          responseWith(mount << Update(id, data))
        }
      }*/
    } ~ get {
      responseWith(mount << Find(All))
    }
  }

  def responseWith(event: Out): StandardRoute = event match {
    case Created(value: Data) =>       complete(StatusCodes.Created, value)
    case Readen(value: Data) =>        complete(StatusCodes.OK, value)
    case Updated(_, newValue: Data) => complete(StatusCodes.OK, newValue)
    case Deleted(_) =>                 complete(StatusCodes.NoContent)

    case Found(_, values: Map[Location, Data]) => complete(StatusCodes.OK, values.values)

    case NotExist =>        complete(StatusCodes.NotFound)
    case AlreadyExist(_) => complete(StatusCodes.Conflict)

    case _: Error =>           complete(StatusCodes.InternalServerError)
    case _ =>                  complete(StatusCodes.InternalServerError)
  }
}
