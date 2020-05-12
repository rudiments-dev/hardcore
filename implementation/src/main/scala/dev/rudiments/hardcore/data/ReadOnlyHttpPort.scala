package dev.rudiments.hardcore.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import dev.rudiments.hardcore.HardPort
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http.Router
import dev.rudiments.hardcore.types.HardID
import io.circe.Encoder

class ReadOnlyHttpPort[T : Encoder](
  prefix: String,
  idDirective: Directive1[HardID[T]],
  override val h: DataSkill[T]
) extends HardPort[DataCommand[T], DataEvent[T]](h) with Router {

  override val routes: Route = pathPrefix(prefix) {
    get {
      pathEndOrSingleSlash {
        responseWith(h(FindAll[T]()))
      } ~ idDirective { id =>
        pathEndOrSingleSlash {
          responseWith(h(Find(id)))
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


