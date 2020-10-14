package dev.rudiments.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.{Event, Message, PortWithoutDependency, Skill}
import dev.rudiments.hardcore.http.query.Directives
import dev.rudiments.hardcore.http.{IDPath, Router, ThingEncoder}
import dev.rudiments.domain.{Domain, Instance, Spec}
import io.circe.Encoder

class ReadOnlyHttpPort(
  prefix: String,
  idField: String,
  override val s: Skill
)(implicit spec: Spec, domain: Domain) extends PortWithoutDependency(s) with Router {

  private implicit val encoder: Encoder[Instance] = new ThingEncoder(domain).specEncoder(spec)
  private val idPath = IDPath(spec.fields(idField).thing)

  override val routes: Route = pathPrefix(prefix) {
    get {
      pathEndOrSingleSlash {
        Directives.query(spec) { query =>
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
  def responseWith(event: Message): StandardRoute = event match {
    case Found(_, value) =>  complete(StatusCodes.OK, value)
    case FoundAll(values) => complete(StatusCodes.OK, values)
    case NotFound(_) =>      complete(StatusCodes.NotFound)
    case _: Error =>         complete(StatusCodes.InternalServerError)
    case _ =>                complete(StatusCodes.InternalServerError)
  }
}


