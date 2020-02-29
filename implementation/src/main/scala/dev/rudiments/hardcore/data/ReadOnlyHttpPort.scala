package dev.rudiments.hardcore.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import dev.rudiments.hardcore.Port
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http.Router
import dev.rudiments.hardcore.types.ID
import io.circe.Encoder

class ReadOnlyHttpPort[T : Encoder](
  prefix: String,
  idDirective: Directive1[ID[T]],
  override val f: DataSkill[T]
) extends Port[DataCommand[T], DataEvent[T]] with Router {

  override val routes: Route = pathPrefix(prefix) {
    get {
      pathEndOrSingleSlash {
        responseWith(f(FindAll[T]()))
      } ~ idDirective { id =>
        pathEndOrSingleSlash {
          responseWith(f(Find(id)))
        }
      }
    }
  }

  import dev.rudiments.hardcore.http.CirceSupport._
  def responseWith(event: DataEvent[T]): StandardRoute = event match {
    case Found(_, value) =>   complete(StatusCodes.OK, value)
    case FoundAll(values) =>  complete(StatusCodes.OK, values)
    case NotFound(_) =>       complete(StatusCodes.NotFound)
    case _: Error =>          complete(StatusCodes.InternalServerError)
  }
}


