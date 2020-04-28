package dev.rudiments.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.Port
import dev.rudiments.hardcore.http.{IDPath, Router, SoftEncoder}
import dev.rudiments.hardcore.types.{ID, Instance, SoftInstance, Type}
import io.circe.Encoder

class ReadOnlyHttpPort(
  prefix: String,
  idField: String,
  override val f: DataSkill
)(implicit t: Type) extends Port[DataCommand, DataEvent] with Router {

  private implicit val encoder: Encoder[Instance] = SoftEncoder(t).contramap( i => i.asInstanceOf[SoftInstance] )
  private val idPath = IDPath(t.fields(idField).kind)(t)

  override val routes: Route = pathPrefix(prefix) {
    get {
      pathEndOrSingleSlash {
        responseWith(f(FindAll))
      } ~ idPath { id =>
        pathEndOrSingleSlash {
          responseWith(f(Find(id)))
        }
      }
    }
  }

  import dev.rudiments.hardcore.http.CirceSupport._
  def responseWith(event: DataEvent): StandardRoute = event match {
    case Found(_, value) =>   complete(StatusCodes.OK, value)
    case FoundAll(values) =>  complete(StatusCodes.OK, values)
    case NotFound(_) =>       complete(StatusCodes.NotFound)
    case _: Error =>          complete(StatusCodes.InternalServerError)
  }
}

