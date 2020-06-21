package dev.rudiments.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.{Event, Failure, Port, Result, Skill, Success}
import dev.rudiments.hardcore.http.query.Directives
import dev.rudiments.hardcore.http.{IDPath, Router, SoftEncoder}
import dev.rudiments.hardcore.types.{ID, Instance, SoftInstance, Type}
import io.circe.Encoder

class ReadOnlyHttpPort(
  prefix: String,
  idField: String,
  override val s: Skill[DataEvent]
)(implicit t: Type) extends Port(s) with Router {

  private implicit val encoder: Encoder[Instance] = SoftEncoder(t).contramap( i => i.asInstanceOf[SoftInstance] )
  private val idPath = IDPath(t.fields(idField).kind)(t)

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
    case Success(Found(_, value)) =>   complete(StatusCodes.OK, value)
    case Success(FoundAll(values)) =>  complete(StatusCodes.OK, values)
    case Failure(NotFound(_)) =>       complete(StatusCodes.NotFound)
    case Failure(_: Error) =>          complete(StatusCodes.InternalServerError)
  }
}


