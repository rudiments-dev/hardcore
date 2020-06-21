package dev.rudiments.hardcore.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import dev.rudiments.hardcore.{Port, Result}
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http.Router
import dev.rudiments.hardcore.types.HardID
import io.circe.Encoder

class ReadOnlyHttpPort[T : Encoder](
  prefix: String,
  idDirective: Directive1[HardID[T]],
  override val s: DataSkill[T]
) extends Port[DataCommand[T], DataEvent[T]](s) with Router {

  override val routes: Route = pathPrefix(prefix) {
    get {
      pathEndOrSingleSlash {
        responseWith(s(FindAll[T]()))
      } ~ idDirective { id =>
        pathEndOrSingleSlash {
          responseWith(s(Find(id)))
        }
      }
    }
  }

  import dev.rudiments.hardcore.http.CirceSupport._
  def responseWith(event: Result[DataEvent[T]]): StandardRoute = event match {
    case Right(Found(_, value)) =>  complete(StatusCodes.OK, value)
    case Right(FoundAll(values)) => complete(StatusCodes.OK, values)
    case Right(_: DataEvent[T]) =>  complete(StatusCodes.OK)
    case Left(NotFound(_)) =>       complete(StatusCodes.NotFound)
    case Left(_: Error) =>          complete(StatusCodes.InternalServerError)
    case _ =>                       complete(StatusCodes.InternalServerError)
  }
}


