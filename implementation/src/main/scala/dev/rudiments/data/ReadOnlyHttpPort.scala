package dev.rudiments.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.{Event, Failure, PortWithoutDependency, Result, Skill, Success}
import dev.rudiments.hardcore.http.query.Directives
import dev.rudiments.hardcore.http.{IDPath, InstanceEncoder, Router}
import dev.rudiments.types.{Instance, Type, TypeSystem}
import io.circe.Encoder

class ReadOnlyHttpPort(
  prefix: String,
  idField: String,
  override val s: Skill[DataEvent]
)(implicit t: Type, typeSystem: TypeSystem) extends PortWithoutDependency(s) with Router {

  private implicit val encoder: Encoder[Instance] = InstanceEncoder(typeSystem).encoder(t)
  private val idPath = IDPath(t.fields(idField).`type`)(t)

  override val routes: Route = pathPrefix(prefix) {
    get {
      pathEndOrSingleSlash {
        Directives.query(t) { query =>
          responseWith(s(FindAll(query)))
        }
      } ~ idPath { id =>
        pathEndOrSingleSlash {
          responseWith(s(Find(id)))
        }
      }
    }
  }

  import dev.rudiments.hardcore.http.CirceSupport._
  def responseWith(event: Result[Event]): StandardRoute = event match {
    case Success(Found(_, value)) =>  complete(StatusCodes.OK, value)
    case Success(FoundAll(values)) => complete(StatusCodes.OK, values)
    case Failure(NotFound(_)) =>      complete(StatusCodes.NotFound)
    case Failure(_: Error) =>         complete(StatusCodes.InternalServerError)
    case _ =>                         complete(StatusCodes.InternalServerError)
  }
}


